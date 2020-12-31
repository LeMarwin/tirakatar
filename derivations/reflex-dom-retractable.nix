{ mkDerivation, base, containers, jsaddle, mtl, ref-tf, reflex
, reflex-dom, stdenv, fetchgit
}:
mkDerivation {
  pname = "reflex-dom-retractable";
  version = "0.1.8.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/ergvein";
    rev = "029eba8e73f313c4236e9e9a377ae37f1213f3de";
    sha256 = "0354g2vwh2j6ckcwxld7kxil1xlmfdiib426xx2245pik6h0scsj";
  };
  postUnpack = "sourceRoot+=/retractable; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers jsaddle mtl ref-tf reflex reflex-dom
  ];
  description = "Routing and retractable back button for reflex-dom";
  license = stdenv.lib.licenses.mit;
}
