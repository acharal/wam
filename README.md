# A simple compiler from Prolog to WAM.

WAM (Warren Abstract Machine) is an efficient popular abstract machine
to compile Prolog logic programs and is implemented in almost 
all industry-strength Prolog compilers such as [SWI-Prolog][1] and [YAP][2].

This interpreter contains the compilation and runtime of WAM instructions
based on the definition in Russinoff 1992 [3]. The interpreter is developed
in Haskell and the purpose of the project is to used as testbed for 
various extensions in WAM.

# Getting Started

The compilation of the WAM interpreter is distributed as a cabal package.
To compile type:

    cabal configure
    cabal build


[1]: http://www.swi-prolog.org/
[2]: http://www.dcc.fc.up.pt/~vsc/Yap/
[3]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.39.873
