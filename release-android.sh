set -xe
gitHash=$(git rev-parse --short HEAD)
releaseArgs="--arg gitHash "\"$gitHash\"" -A android.tirakatar-app --arg release true"
nix-build $releaseArgs -o android-release "$@"
nix-build $releaseArgs -o android-release-apk --arg releaseBundle false "$@"
