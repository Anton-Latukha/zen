{ mkDerivation, base, hsyslog, optparse-applicative, stdenv, unix
}:
mkDerivation {
  pname = "zen";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hsyslog optparse-applicative unix
  ];
  homepage = "https://blog.latukha.com";
  description = "Synopsis text";
  license = stdenv.lib.licenses.gpl3;
}
