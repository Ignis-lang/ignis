# Prebuilt-binary derivation for the Ignis compiler.
#
# Pulls the matching tarball from the GitHub Release named by
# ./release-info.nix (or ./nightly-info.nix) and installs the compiler and the
# standard library it shipped with into a Nix store path, so consumers do not
# run the bootstrap ladder.
#
# Supported systems: x86_64-linux. Other systems fall back to the source build
# (see flake.nix).

{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  # The C toolchain the compiler calls to compile and link programs. Passed in
  # by flake.nix so it matches the source build in default.nix.
  runtimeTools,
  # Which channel pointer to read. Pass ./nightly-info.nix for the rolling
  # nightly package.
  infoFile ? ./release-info.nix,
}:

let
  releaseInfo = import infoFile;
  system = stdenv.hostPlatform.system;
  artifact = releaseInfo.artifacts.${system} or (throw (
    "ignis: no prebuilt binary published for system '${system}'. "
    + "Use the source build (packages.ignis-source) instead."
  ));

  # A nightly installs under its own binary and data names so a stable and a
  # nightly package can coexist in one profile without colliding on `bin/ignis`.
  isNightly = lib.hasInfix "nightly" releaseInfo.version;
  appId = if isNightly then "ignis-nightly" else "ignis";
in
stdenv.mkDerivation {
  pname = appId;
  version = releaseInfo.version;

  src = fetchurl {
    inherit (artifact) url hash;
  };

  # The tarball expands into the working directory with no top-level folder.
  sourceRoot = ".";
  unpackPhase = ''
    runHook preUnpack
    mkdir -p source
    tar -xzf $src -C source
    runHook postUnpack
  '';

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  # The binary links libc, libm and libgcc_s only.
  buildInputs = [ stdenv.cc.cc.lib ];

  # Releases up to v0.4.0 ship a C runtime that the compiler links as
  # std/runtime/libignis_rt.a but do not ship the archive itself. Later std
  # trees have no C runtime and no Makefile, so there is nothing to build.
  buildPhase = ''
    runHook preBuild

    if [ -f source/std/runtime/Makefile ]; then
      make -C source/std/runtime libignis_rt.a
      rm -f source/std/runtime/internal/*.o
    fi

    runHook postBuild
  '';

  dontStrip = true;

  installPhase = ''
    runHook preInstall

    cd source

    install -Dm755 ignis $out/lib/${appId}/ignis-bin

    mkdir -p $out/share/${appId}
    cp -r --no-preserve=mode std $out/share/${appId}/std

    makeWrapper $out/lib/${appId}/ignis-bin $out/bin/${appId} \
      --set-default IGNIS_STD_PATH "$out/share/${appId}/std" \
      --prefix PATH : "${lib.makeBinPath runtimeTools}"

    runHook postInstall
  '';

  doInstallCheck = true;

  # Exercise the installed wrapper end to end: the shipped std, codegen, the C
  # toolchain on its PATH, and the runtime of a linked program.
  installCheckPhase = ''
    runHook preInstallCheck

    $out/bin/${appId} --version

    checkDir="$(mktemp -d)"
    cat > "$checkDir/hello.ign" << 'EOF'
    import Io from "std::io";

    function main(): void {
      Io::println("Hello, Ignis!");
      return;
    }
    EOF

    (
      cd "$checkDir"
      $out/bin/${appId} build hello.ign -o hello

      # Releases up to v0.4.0 treat `-o` as an output directory.
      program=./hello
      if [ -d hello ]; then
        program=./hello/hello
      fi

      test "$($program)" = "Hello, Ignis!"
    )

    runHook postInstallCheck
  '';

  meta = with lib; {
    description = "The Ignis programming language compiler (prebuilt binary)";
    homepage = "https://github.com/Ignis-lang/ignis";
    license = licenses.gpl3Only;
    mainProgram = appId;
    platforms = builtins.attrNames releaseInfo.artifacts;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
  };
}
