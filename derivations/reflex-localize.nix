{ mkDerivation, base, jsaddle, mtl, reflex, reflex-external-ref
, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "reflex-localize";
  version = "1.0.4.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/ergvein";
    rev = "029eba8e73f313c4236e9e9a377ae37f1213f3de";
    sha256 = "0354g2vwh2j6ckcwxld7kxil1xlmfdiib426xx2245pik6h0scsj";
  };
  postUnpack = "sourceRoot+=/reflex-localize; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base jsaddle mtl reflex reflex-external-ref text
  ];
  homepage = "https://github.com/hexresearch/ergvein";
  description = "Localization library for reflex";
  license = stdenv.lib.licenses.mit;
}
