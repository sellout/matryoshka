with import <nixpkgs> { };

runCommand "dummy" {
  buildInputs = [sbt];
  shellHook = ''
    sbt
    exit
    '';
}

""
