{ mkDerivation, base, ef, excelsior, pure-core, pure-default, pure-lifted, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-router";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ef excelsior pure-core pure-default pure-lifted pure-txt ];
  homepage = "github.com/grumply/pure-router";
  license = stdenv.lib.licenses.bsd3;
}
