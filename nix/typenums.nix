{ mkDerivation, base, fetchgit, hpack, hspec, QuickCheck, stdenv }:
mkDerivation {
  pname = "typenums";
  version = "0.1.1.1";
  src = fetchgit {
    url = "https://github.com/lightandlight/typenums";
    sha256 = "0c61ga1lkwfqr31xpmcyfgipv99slxasmdzbv6xipm0vw6a4y98j";
    rev = "4b3d3d3c0dc30e6de131a8a70006a1a21ac1be72";
  };
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base hspec QuickCheck ];
  preConfigure = "hpack";
  homepage = "https://github.com/adituv/typenums#readme";
  description = "Type level numbers using existing Nat functionality";
  license = stdenv.lib.licenses.bsd3;
}
