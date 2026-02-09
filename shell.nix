{ pkgs ? import <nixpkgs> { } }:

let
  ignis = import ./default.nix { inherit pkgs; };
in
ignis.shell
