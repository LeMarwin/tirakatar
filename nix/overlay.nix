self: super:
let project = import ../default.nix {};
in rec {
  tirakatar-app = project.ghc.tirakatar-app;
}
