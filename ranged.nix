{ mkDerivation, base, inspection-testing, lens, stdenv, typenums }:
mkDerivation {
  pname = "ranged";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base inspection-testing lens typenums ];
  license = stdenv.lib.licenses.bsd3;
}
