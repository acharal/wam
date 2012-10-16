# A Simple Prolog Compiler to WAM

[WAM (Warren Abstract Machine)][1] is an efficient popular abstract machine
to compile Prolog logic programs and is implemented in almost 
all industrial-strength Prolog compilers such as [SWI-Prolog][2] and [YAP][3].

This interpreter contains the compilation and runtime of WAM instructions
based on the definition in [Russinoff 1992][4]. The interpreter is developed
in Haskell and the purpose of the project is to be used as testbed for various
extensions in WAM.

# Getting Started

The compilation of the WAM interpreter is distributed as a cabal package.
To compile type:

    cabal configure
    cabal build

# Features and Limitations

Compilations supports some basic optimizations such as

 * tail execution optimization
 * unsafe variables

## Limitations

There are also some features of WAM that are not yet supported such as

 * There are no special instructions for lists (get_list, put_list).
 * No environment trimming.
 * No indexing at the switch-operators
 * No cut functionality

## Other limitations

 * No garbage collection has been implemented.
 * A subset of the ISO-Prolog is supported and compiled. No build-in predicates
   are supported, neither user-defined operators.

[1]: http://wambook.sourceforge.net/
[2]: http://www.swi-prolog.org/
[3]: http://www.dcc.fc.up.pt/~vsc/Yap/
[4]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.39.873
