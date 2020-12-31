#! /usr/bin/env nix-shell
#!nix-shell -i bash -p androidenv.androidPkgs_9_0.platform-tools

adb -d uninstall org.tirakatar
adb install -r ./android-result/debug/android-app-debug.apk
