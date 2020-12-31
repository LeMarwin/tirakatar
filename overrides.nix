# Here you can put overrides of dependencies
{ reflex-platform
, gitHash
, versionTag
}:
let
  pkgs = reflex-platform.nixpkgs;
  overrideCabal = pkgs.haskell.lib.overrideCabal;
  enableCabalFlag = pkgs.haskell.lib.enableCabalFlag;
  disableCabalFlag = pkgs.haskell.lib.disableCabalFlag;
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  lib = pkgs.haskell.lib;
  dontHaddock = lib.dontHaddock;
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "siers";
    repo = "nix-gitignore";
    rev = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ingnoreGarbage = pkg: overrideCabal pkg (pkg : let
    ignore-list = ''
      /.ghc.environment*
      /dist-newstyle
      /android-release
      /android-result
    '';
    in { src = gitignore.gitignoreSourceAux ignore-list pkg.src; } );
  addVersions = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    preConfigure = (drv.preConfigure or "") + ''
      ${if gitHash == null then "" else "export GIT_HASH=${gitHash}"}
      export VERSION_TAG=${versionTag}
    '';
  });
in (self: super: let
  # Internal packages (depends on production or dev environment)
  callInternal = name: path: args: (
    dontHaddock ( self.callCabal2nix name (ingnoreGarbage path) args ));
  isAndroid = self.ghc.stdenv.targetPlatform.isAndroid;
  tirOpts = if isAndroid then "-fandroid --no-haddock" else "--no-haddock";
  dontProfile = drv: disableCabalFlag drv "profile-reflex";
  in {
    # Internal
    tirakatar-common = ingnoreGarbage super.tirakatar-common;
    tirakatar-crypto = ingnoreGarbage super.tirakatar-crypto;
    tirakatar-app = addVersions (ingnoreGarbage (super.callCabal2nixWithOptions "tirakatar-app" ./tirakatar tirOpts {}));
    tirakatar-android = ingnoreGarbage (super.callCabal2nixWithOptions "tirakatar-android" ./android tirOpts {});
    tirakatar-desktop = ingnoreGarbage super.tirakatar-desktop;
    tirakatar-native = ingnoreGarbage super.tirakatar-native;
    tirakatar-types = ingnoreGarbage super.tirakatar-types;
    tirakatar-version = ingnoreGarbage super.tirakatar-version;
    # Overridess
    android-activity = self.callPackage ./derivations/android-activity.nix {
      inherit (pkgs.buildPackages) jdk;
    };
    bytestring-trie = self.callPackage ./derivations/bytestring-trie.nix {};
    clay = self.callPackage ./derivations/clay.nix {};
    criterion = lib.dontCheck super.criterion;
    cryptonite = self.callPackage ./derivations/cryptonite.nix {};
    doctemplates = self.callPackage ./derivations/doctemplates.nix {};
    flat = lib.dontCheck (super.flat);
    haddock-library = self.callPackage ./derivations/haddock-library.nix {};
    haskoin-core = self.callPackage ./derivations/haskoin-core.nix {};
    hp2any-core = self.callPackage ./derivations/hp2any-core.nix {};
    hp2any-graph = self.callPackage ./derivations/hp2any-graph.nix {};
    hslua = self.callPackage ./derivations/hslua.nix {};
    HsYAML = self.callPackage ./derivations/HsYAML.nix {};
    immortal-worker = self.callPackage ./derivations/immortal-worker.nix {};
    iproute = self.callPackage ./derivations/iproute.nix {};
    lmdb = self.callPackage ./derivations/haskell-lmdb.nix {};
    pandoc = self.callPackage ./derivations/pandoc.nix {};
    pandoc-citeproc = self.callPackage ./derivations/pandoc-citeproc.nix {};
    pandoc-types = self.callPackage ./derivations/pandoc-types.nix {};
    parseargs = lib.dontCheck super.parseargs;
    reflex = enableCabalFlag super.reflex "O2";
    reflex-dom-core = dontProfile (lib.dontCheck (super.reflex-dom-core));
    reflex-dom-retractable = self.callPackage ./derivations/reflex-dom-retractable.nix {};
    reflex-external-ref = self.callPackage ./derivations/reflex-external-ref.nix {};
    reflex-localize = self.callPackage ./derivations/reflex-localize.nix {};
    reflex-localize-dom = self.callPackage ./derivations/reflex-localize-dom.nix {};
    secp256k1-haskell = self.callPackage ./derivations/secp256k1-haskell.nix {};
    skylighting = self.callPackage ./derivations/skylighting.nix {};
    skylighting-core = self.callPackage ./derivations/skylighting-core.nix {};
    stm-hamt = self.callPackage ./derivations/stm-hamt.nix {};
    texmath = self.callPackage ./derivations/texmath.nix {};
    tls = lib.dontCheck super.tls;
    zlib = self.callPackage ./derivations/zlib.nix {};
  }
)
