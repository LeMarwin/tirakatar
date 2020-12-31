set -xe
cabal new-build tirakatar --enable-profiling
hp2any-graph -e ../dist-newstyle/build/x86_64-linux/ghc-8.6.5/tirakatar-app-0.1.0.0/x/tirakatar/build/tirakatar/tirakatar -- +RTS -hc -i0.02 -L256
