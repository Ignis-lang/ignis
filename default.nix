{
  pkgs ? import <nixpkgs> { },
  rustPlatform ? pkgs.rustPlatform,
  version ? "0.4.0",
}:

let
  runtimeTools = with pkgs; [
    gcc
    binutils
    gnumake
  ];

  runtimeToolsPath = pkgs.lib.makeBinPath runtimeTools;

  fullSrc = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      (builtins.match ".*\\.git$" path) == null &&
      (builtins.match ".*\\.direnv$" path) == null &&
      (builtins.match ".*target$" path) == null &&
      (builtins.match ".*build$" path) == null;
  };

  package = rustPlatform.buildRustPackage {
    pname = "ignis";
    inherit version;
    src = fullSrc;

    cargoLock = {
      lockFile = ./Cargo.lock;
    };

    nativeBuildInputs = with pkgs; [
      makeWrapper
      pkg-config
    ];

    doCheck = false;
    cargoBuildFlags = [ "-p" "ignis" ];

    postInstall = ''
      mkdir -p $out/lib/ignis
      mv $out/bin/ignis $out/lib/ignis/ignis-bin

      mkdir -p $out/share/ignis
      cp -r std $out/share/ignis/std
      chmod -R u+w $out/share/ignis/std

      makeWrapper $out/lib/ignis/ignis-bin $out/bin/ignis \
        --set-default IGNIS_STD_PATH "$out/share/ignis/std" \
        --prefix PATH : "${runtimeToolsPath}"
    '';

    meta = with pkgs.lib; {
      description = "The Ignis programming language compiler";
      homepage = "https://github.com/Ignis-lang/ignis";
      license = licenses.gpl3Only;
      platforms = platforms.linux;
      mainProgram = "ignis";
    };
  };
in
{
  inherit runtimeTools runtimeToolsPath package;

  shell = pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      git
      rustup
      rust-analyzer
      pkg-config
      gdb
      lldb
      valgrind
    ] ++ runtimeTools;

    shellHook = ''
      export CARGO_HOME="$HOME/.cargo"
      export RUSTUP_HOME="$HOME/.rustup"
      export IGNIS_HOME="$PWD"
      export IGNIS_STD_PATH="$IGNIS_HOME/std"
      export RUST_BACKTRACE=1

      if command -v rustup >/dev/null 2>&1; then
        rustup default nightly >/dev/null 2>&1 || true
      fi

      if command -v rustc >/dev/null 2>&1; then
        rustRelease="$(rustc -vV | sed -n 's/^release: //p')"
        if [ -n "$rustRelease" ]; then
          export CARGO_TARGET_DIR="$PWD/target/$rustRelease"
        fi
      fi

      echo "Ignis development environment loaded"
      echo "Rust toolchain is provided via rustup to keep cargo, rustc, and cargo-clippy aligned"
    '';
  };
}
