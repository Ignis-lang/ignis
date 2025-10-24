{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  name = "ignis-dev-shell";

  nativeBuildInputs = with pkgs; [
    git
    cargo
    rustc
    gcc
    tinycc
    valgrind
    gdb
    lldb
    pkg-config
    rustup
  ];

  shellHook = ''
    export CARGO_HOME="$HOME/.cargo"
    export RUSTUP_HOME="$HOME/.rustup"
    export IGNIS_HOME="."
    export IGNIS_STD_PATH="$IGNIS_HOME/std"
    export RUST_BACKTRACE=1
    export PKG_CONFIG_PATH="${pkgs.libffi.dev}/lib/pkgconfig"

    rustup default nightly
  '';
}
