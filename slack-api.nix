{ mkDerivation, aeson, base, bytestring, containers, errors
, hashable, io-streams, lens, lens-aeson, monad-loops, mtl, network
, network-uri, scientific, stdenv, text, time, time-locale-compat
, tls, transformers, websockets, wreq, wuss
}:

let
  lib         = stdenv.lib;
  isWithin    = p: dirPath: lib.hasPrefix (toString dirPath) (toString p);
  cabalFilter = path: type: (let pathBaseName = baseNameOf path; in
                               !(lib.hasSuffix "~" pathBaseName) &&
                               !(lib.hasSuffix "#" pathBaseName) &&
                               !(lib.hasPrefix "." pathBaseName) &&
                               (
                                   pathBaseName == "slack-api.cabal" ||
                                   pathBaseName == "LICENSE"              ||
                                   pathBaseName == "Setup.hs"             ||
                                   isWithin path ./src                    ||
                                   false
                               )
                            );
in

mkDerivation {
  pname = "slack-api";
  version = "0.12";
  src = builtins.filterSource cabalFilter ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers errors hashable io-streams lens
    lens-aeson monad-loops mtl network network-uri scientific text time
    time-locale-compat tls transformers websockets wreq wuss
  ];
  testHaskellDepends = [ base ];
  description = "Bindings to the Slack RTM API";
  license = stdenv.lib.licenses.mit;
}
