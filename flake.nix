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
    let
      releaseInfo = import ./nix/release-info.nix;

      # Systems that ship a prebuilt binary in the matching GitHub Release.
      # Other systems still get the source build.
      prebuiltSystems = builtins.attrNames releaseInfo.artifacts;

      perSystem = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };

          ignisNix = import ./default.nix {
            inherit pkgs;
            version = "0.4.0";
          };

          # Built from the C seed through the bootstrap ladder, no download.
          ignisSource = ignisNix.package;

          hasPrebuilt = builtins.elem system prebuiltSystems;

          ignisBin =
            if hasPrebuilt then
              pkgs.callPackage ./nix/binary.nix { inherit (ignisNix) runtimeTools; }
            else
              null;

          # Rolling nightly prebuilt. On `main` the hash in nightly-info.nix is
          # a placeholder: consume it via
          # `github:Ignis-lang/ignis/nightly#ignis-nightly`.
          ignisNightly =
            if hasPrebuilt then
              pkgs.callPackage ./nix/binary.nix {
                inherit (ignisNix) runtimeTools;
                infoFile = ./nix/nightly-info.nix;
              }
            else
              null;

          # Prefer the prebuilt binary when one exists for this system.
          ignisDefault = if hasPrebuilt then ignisBin else ignisSource;
        in
        {
          # Packages:
          #   .default        -> prebuilt when available, source otherwise
          #   .ignis          -> alias for .default
          #   .ignis-bin      -> explicit prebuilt (only on supported systems)
          #   .ignis-source   -> explicit source build (bootstrap ladder)
          #   .ignis-nightly  -> rolling nightly prebuilt (pin to the nightly ref)
          packages = {
            default = ignisDefault;
            ignis = ignisDefault;
            ignis-source = ignisSource;
          } // (if hasPrebuilt then {
            ignis-bin = ignisBin;
            ignis-nightly = ignisNightly;
          } else { });

          apps = {
            default = flake-utils.lib.mkApp {
              drv = ignisDefault;
              exePath = "/bin/ignis";
            };

            ignis = flake-utils.lib.mkApp {
              drv = ignisDefault;
              exePath = "/bin/ignis";
            };
          } // (if hasPrebuilt then {
            ignis-nightly = flake-utils.lib.mkApp {
              drv = ignisNightly;
              exePath = "/bin/ignis-nightly";
            };
          } else { });

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
                # The `--target qbe` backend lowers Ignis LIR to QBE IL and
                # runs this tool to get assembly.
                pkgs.qbe
              ];

            shellHook = ''
              export IGNIS_HOME="$PWD"
              export IGNIS_STD_PATH="$IGNIS_HOME/std"

              echo "Ignis development environment loaded (Nix flake)"
            '';
          };

          formatter = pkgs.nixpkgs-fmt;
        });
    in
    perSystem // {
      # Overlay for downstream consumers:
      #
      #   nixpkgs.overlays = [ inputs.ignis.overlays.default ];
      #   environment.systemPackages = [ pkgs.ignis ];
      #
      # `pkgs.ignis`         -> prebuilt binary when available, source otherwise
      # `pkgs.ignis-source`  -> built from the C seed
      # `pkgs.ignis-bin`     -> explicit prebuilt (only on prebuilt systems)
      # `pkgs.ignis-nightly` -> rolling nightly prebuilt (only on prebuilt systems)
      overlays.default = final: prev:
        let
          system = prev.stdenv.hostPlatform.system;
          hasSystem = perSystem.packages ? ${system};
          sysPkgs = perSystem.packages.${system};
        in
        if hasSystem then
          {
            ignis = sysPkgs.ignis;
            ignis-source = sysPkgs.ignis-source;
          }
          // nixpkgs.lib.optionalAttrs (sysPkgs ? ignis-bin) {
            ignis-bin = sysPkgs.ignis-bin;
          }
          // nixpkgs.lib.optionalAttrs (sysPkgs ? ignis-nightly) {
            ignis-nightly = sysPkgs.ignis-nightly;
          }
        else
          { };
    };
}
