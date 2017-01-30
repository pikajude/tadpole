{ mkDerivation, base, gloss, lens, reflex, reflex-gloss, stdenv, cabal-install }:
mkDerivation {
  pname = "tadpole";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gloss lens reflex reflex-gloss ];
  buildTools = [ cabal-install ];
  description = "A little game";
  license = stdenv.lib.licenses.mit;
}
