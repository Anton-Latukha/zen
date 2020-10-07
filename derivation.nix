{ mkDerivation, base, errors, hsyslog, optparse-applicative
, process, stdenv, unix
}:
mkDerivation {
  pname = "zen";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base errors hsyslog optparse-applicative process unix
  ];
  executableHaskellDepends = [
    base errors hsyslog optparse-applicative process unix
  ];
  doHaddock = false;
  homepage = "https://blog.latukha.com";
  description = "Synopsis text";
  license = stdenv.lib.licenses.gpl3;
}
