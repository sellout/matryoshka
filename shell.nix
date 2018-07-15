{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  buildInputs = [
    git
    nodejs
    sbt
  ];

  name = "turtles";
}
