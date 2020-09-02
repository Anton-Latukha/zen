{ mkDerivation, base, optparse-applicative, stdenv }:
mkDerivation {
  pname = "zen";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base optparse-applicative ];
  homepage = "https://blog.latukha.com";
  description = "Synopsis text";
  license = stdenv.lib.licenses.gpl3;
}
