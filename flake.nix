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

        rustToolchain = pkgs.pkgsBuildHost.rust-bin.nightly.latest.default.override {
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
