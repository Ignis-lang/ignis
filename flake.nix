{
  description = "Ignis compiler and development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        # Pinned to the exact date ci.yml installs. `nightly.latest` floats, so
        # the shell and CI drifted apart silently and a lint that existed in only
        # one of them decided whether a run was red. Moving this means moving
        # RUST_NIGHTLY in ci.yml and nightly.yml in the same commit.
        rustToolchain = pkgs.pkgsBuildHost.rust-bin.nightly."2026-04-22".default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        rustPlatform = pkgs.makeRustPlatform {
          cargo = rustToolchain;
          rustc = rustToolchain;
        };

        ignisNix = import ./default.nix {
          inherit pkgs rustPlatform;
          version = "0.4.0";
        };
      in
      {
        packages.default = ignisNix.package;
        packages.ignis = ignisNix.package;

        apps.default = flake-utils.lib.mkApp {
          drv = ignisNix.package;
          exePath = "/bin/ignis";
        };

        apps.ignis = flake-utils.lib.mkApp {
          drv = ignisNix.package;
          exePath = "/bin/ignis";
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs =
            ignisNix.runtimeTools
            ++ [
              pkgs.git
              pkgs.pkg-config
              rustToolchain
              # Selected for the Linux targets by `.cargo/config.toml`; without
              # it on PATH every link fails.
              pkgs.mold
              # Process-per-test runner. Prefer `cargo nextest run` over
              # `cargo test`: it reports every failing target instead of
              # stopping at the first, which matters for a suite whose e2e
              # tests each shell out to gcc. Doctests are not covered — those
              # still need `cargo test --doc`.
              pkgs.cargo-nextest
              # Lints the workflows the same way CI does.
              pkgs.actionlint
            ];

          shellHook = ''
            export IGNIS_HOME="$PWD"
            export IGNIS_STD_PATH="$IGNIS_HOME/std"
            export RUST_BACKTRACE=1

            if command -v rustc >/dev/null 2>&1; then
              rustRelease="$(rustc -vV | sed -n 's/^release: //p')"
              if [ -n "$rustRelease" ]; then
                export CARGO_TARGET_DIR="$PWD/target/$rustRelease"
              fi
            fi

            echo "Ignis development environment loaded (Nix flake)"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
