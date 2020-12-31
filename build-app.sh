gitHash=$(git rev-parse --short HEAD)
nix-build -A ghc.tirakatar-app --arg gitHash "\"$gitHash\""
