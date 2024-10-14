{
  pkgs ? import <nixpkgs> { },
}:
let
  homeDir = builtins.getEnv "HOME";
in
{
  default = pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      git
      cargo
      rustc
      gcc
      tinycc
      valgrind
      gdb
    ];

    CARGO_HOME = "${homeDir}/.cargo";
    RUSTUP_HOME = "${homeDir}/.rustup";
    IGNIS_HOME = "${homeDir}/.ignis";
    RUST_BACKTRACE = 1;
  };
}
