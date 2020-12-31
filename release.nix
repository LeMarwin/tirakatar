let project = import ./default.nix { release = false; };
    /* containers = import ./nix/containers.nix {
      isProd = true;
      gitHash = ?;
      gitTag = ?;
      gitBranch = ?;
      containerTag = ?;
    }; */
in {
  tirakatar-desktop = project.ghc.tirakatar-app;
  tirakatar-android = project.android.tirakatar-app;
}
