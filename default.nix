with import <nixpkgs> {}; {
    profunEnv = stdenv.mkDerivation {
        name = "ttt-env";
        buildInputs = [ stdenv
                        # Haskell dependencies
                        ghc
                        stack
                        cabal-install
                        mesa
                        freeglut
                      ];
        LD_LIBRARY_PATH="${mesa}/lib:${freeglut}/lib";
        pure = false;

    };
}