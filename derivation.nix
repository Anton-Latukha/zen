{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "zen";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "https://blog.latukha.com";
  description = "Synopsis text";
  license = stdenv.lib.licenses.gpl3;
}
