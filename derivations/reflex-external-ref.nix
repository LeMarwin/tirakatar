{ mkDerivation, base, deepseq, reflex, stdenv, fetchgit }:
mkDerivation {
  pname = "reflex-external-ref";
  version = "1.0.3.1";
  src = fetchgit {
    url = "https://github.com/hexresearch/ergvein";
    rev = "029eba8e73f313c4236e9e9a377ae37f1213f3de";
    sha256 = "0354g2vwh2j6ckcwxld7kxil1xlmfdiib426xx2245pik6h0scsj";
  };
  postUnpack = "sourceRoot+=/reflex-external-ref; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base deepseq reflex ];
  homepage = "https://github.com/hexresearch/ergvein";
  description = "External reference with reactivity support";
  license = stdenv.lib.licenses.mit;
}
