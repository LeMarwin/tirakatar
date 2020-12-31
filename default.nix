{ release ? false
, profile ? false
, gitHash ? null
, releaseBundle ? true
 }:
let
   reflex-platform = import ./platform-overlay.nix { inherit profile; };
   version = import ./android-version.nix;
   versionTag = version.code;
   project = reflex-platform.project ({ pkgs, ... }: {
    packages = {
      tirakatar-common = ./common;
      tirakatar-crypto = ./crypto;
      tirakatar-app = ./tirakatar;
      tirakatar-android = ./android;
      tirakatar-desktop = ./desktop;
      tirakatar-native = ./native;
      tirakatar-types = ./types;
      tirakatar-version = ./version;
    };
    shells = {
      ghc = [
        "tirakatar-common"
        "tirakatar-crypto"
        "tirakatar-desktop"
        "tirakatar-native"
        "tirakatar-types"
        "tirakatar-version"
        "tirakatar-app"
      ];
    };
    overrides = import ./overrides.nix { inherit reflex-platform gitHash versionTag; };

    shellToolOverrides = ghc: super: {
      inherit (pkgs) leveldb;
      inherit (pkgs.haskellPackages) hakyll;
      hp2any-graph = if profile then ghc.hp2any-graph else null;
      ghc-prof-flamegraph = if profile then ghc.ghc-prof-flamegraph else null;
    };

    android.tirakatar-app = {
      executableName = "tirakatar";
      applicationId = "org.tirakatar";
      displayName = "Tirakatar";
      resources = ./tirakatar/static/res;
      assets = ./tirakatar/static/assets;
      iconPath = "@drawable/ic_launcher";
      runtimeSharedLibs = nixpkgs: [
        "${nixpkgs.zlibSys}/lib/libz.so"
        "${nixpkgs.secp256k1Sys}/lib/libsecp256k1.so"
        "${nixpkgs.lmdbSys}/lib/liblmdb.so"
      ];
      /* additionalDependencies = ''

      ''; */
      javaSources = [
        ./tirakatar/java
        "${project.ghc.x509-android.src}/java"
      ];
      version = version;
      services = ''
      <provider
          android:name="androidx.core.content.FileProvider"
          android:authorities="org.tirakatar.fileprovider"
          android:exported="false"
          android:grantUriPermissions="true">
          <meta-data
              android:name="android.support.FILE_PROVIDER_PATHS"
              android:resource="@xml/file_paths" />
      </provider>
      '';
    };

  });
  in project
