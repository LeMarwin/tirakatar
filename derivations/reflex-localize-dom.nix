{ mkDerivation, base, containers, reflex, reflex-dom
, reflex-localize, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "reflex-localize-dom";
  version = "1.0.2.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/ergvein";
    rev = "029eba8e73f313c4236e9e9a377ae37f1213f3de";
    sha256 = "0354g2vwh2j6ckcwxld7kxil1xlmfdiib426xx2245pik6h0scsj";
  };
  postUnpack = "sourceRoot+=/reflex-localize-dom; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers reflex reflex-dom reflex-localize text
  ];
  homepage = "https://github.com/hexresearch/ergvein";
  description = "Helper widgets for reflex-localize";
  license = stdenv.lib.licenses.mit;
}
