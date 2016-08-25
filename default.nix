{ mkDerivation, base, bytestring, data-default, exceptions, hspec
, HTTP, http-types, stdenv, time, transformers, unix, wai, warp
}:
mkDerivation {
  pname = "warp-autoquit";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-default exceptions http-types transformers
    unix wai
  ];
  testHaskellDepends = [
    base bytestring data-default hspec HTTP http-types time
    transformers wai warp
  ];
  description = "Automatically quit warp server when inactive";
  license = stdenv.lib.licenses.bsd3;
}
