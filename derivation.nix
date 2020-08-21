{ mkDerivation, base, stdenv, unicode-prelude }:
mkDerivation {
  pname = "zen";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "";
  description = "Synopsis text";
  license = stdenv.lib.licenses.gpl3;
}
