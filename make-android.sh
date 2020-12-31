gitHash=$(git rev-parse --short HEAD)
nix-build --arg isAndroid true --arg gitHash "\"$gitHash\"" -A android.tirakatar-app -o android-result "$@"
