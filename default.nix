{
  pkgs ? import <nixpkgs> { },
  version ? "0.4.0",
}:

let
  runtimeTools = with pkgs; [
    gcc
    binutils
    gnumake
  ];

  runtimeToolsPath = pkgs.lib.makeBinPath runtimeTools;

  # Only top-level build outputs are excluded: `ignis/build/` is compiler
  # source and must stay in.
  fullSrc = pkgs.lib.cleanSourceWith {
    src = ./.;
    filter = path: type:
      let
        relative = pkgs.lib.removePrefix (toString ./. + "/") (toString path);
      in
      !(builtins.elem relative [ ".git" ".direnv" "build" "target" "target-local" "result" ]);
  };

  # The compiler is built the way scripts/bootstrap.sh builds it, with no
  # prebuilt compiler: stage0 from the C seed in bootstrap/seed, stage1 is
  # ignis/main.ign compiled by stage0, and stage2, the binary a release ships,
  # is the same sources compiled by stage1.
  package = pkgs.stdenv.mkDerivation {
    pname = "ignis";
    inherit version;
    src = fullSrc;

    nativeBuildInputs = with pkgs; [
      makeWrapper
      xz
    ];

    buildPhase = ''
      runHook preBuild

      patchShebangs scripts/build_from_seed.sh

      root="$PWD"

      # The selfhost driver writes its emitted C and object file into the
      # working directory, so each stage runs in its own.
      compileStage() {
        mkdir -p "$root/stages/$2"
        (
          cd "$root/stages/$2"
          IGNIS_STD_PATH="$root/std" "$1" "$root/ignis/main.ign" -o "$root/stages/$2/ignis"
        )
      }

      stage0="$(scripts/build_from_seed.sh -o "$root/stages/stage0/ignis")"
      compileStage "$stage0" stage1
      compileStage "$root/stages/stage1/ignis" stage2

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      install -Dm755 stages/stage2/ignis $out/lib/ignis/ignis-bin

      mkdir -p $out/share/ignis
      cp -r std $out/share/ignis/std
      chmod -R u+w $out/share/ignis/std

      makeWrapper $out/lib/ignis/ignis-bin $out/bin/ignis \
        --set-default IGNIS_STD_PATH "$out/share/ignis/std" \
        --prefix PATH : "${runtimeToolsPath}"

      runHook postInstall
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
      xz
      python3
      gdb
      lldb
      valgrind
    ] ++ runtimeTools;

    shellHook = ''
      export IGNIS_HOME="$PWD"
      export IGNIS_STD_PATH="$IGNIS_HOME/std"

      echo "Ignis development environment loaded"
    '';
  };
}
