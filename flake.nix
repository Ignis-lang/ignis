{
  description = "Ignis compiler and development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        ignisNix = import ./default.nix {
          inherit pkgs;
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
              # scripts/build_from_seed.sh decompresses the C seed.
              pkgs.xz
              # The bootstrap ladder, its gates and the parity harnesses.
              pkgs.python3
              # Lints the workflows the same way CI does.
              pkgs.actionlint
            ];

          shellHook = ''
            export IGNIS_HOME="$PWD"
            export IGNIS_STD_PATH="$IGNIS_HOME/std"

            echo "Ignis development environment loaded (Nix flake)"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
