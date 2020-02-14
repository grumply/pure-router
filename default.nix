{ mkDerivation, base, ef, excelsior, pure-core, pure-default, pure-events, pure-html, pure-lifted, pure-txt, pure-uri, stdenv
}:
mkDerivation {
  pname = "pure-router";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ef excelsior pure-core pure-default pure-events pure-html pure-lifted pure-txt pure-uri ];
  homepage = "github.com/grumply/pure-router";
  license = stdenv.lib.licenses.bsd3;
}
