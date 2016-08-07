# A Simple PROLOG Compiler to WAM

[![GitHub license](https://img.shields.io/badge/license-GPLv2-blue.svg)](https://raw.githubusercontent.com/acharal/wam/master/LICENSE)
[![Build Status](https://travis-ci.org/acharal/wam.svg?branch=master)](https://travis-ci.org/acharal/wam)


[WAM][1] (Warren Abstract Machine) is an efficient popular abstract machine
to compile Prolog logic programs and is implemented in almost 
all industrial-strength PROLOG compilers such as [SWI-Prolog][2] and [YAP][3].

This interpreter contains the compilation and runtime of WAM instructions
based on the definition in [Russinoff 1992][4]. The interpreter is developed
in Haskell and the purpose of the project is to be used as testbed for various
extensions in WAM.

# Getting Started


## Building

To build the WAM compiler you should have a system with GHC and cabal.
The following sequence of commands will build the compiler after installing
any dependencies needed.
```bash
$ cabal update
$ cabal install --only-dependencies
$ cabal configure && cabal build
```

## Compile an example to WAM

After a successfull build the compiler will reside by default in `./dist/build/wam/wam`.
You can add the directory to your current path, i.e.
```bash
$ export PATH=`pwd`/dist/build/wam:$PATH
```
To compile a PROLOG program to WAM you should issue:
```bash
$ wam -c -i prog.pl -o output.wam
```
where `prog.pl` is your initial PROLOG source file, `output.wam` is the
name of the output file that will contain WAM bytecode. The switch `-c` will
only compile the program but not run. On the other hand if you omit `-c`,
namely
```bash
$ wam -i prog.pl -o output.wam
```
the program will also be executed after the compilation.

# Features and Limitations

Compilations supports some basic optimizations such as

 * tail execution optimization
 * unsafe variables

## Limitations

There are also some features of WAM that are not yet supported such as:

 * There are no special instructions for lists (get_list, put_list).
 * No environment trimming.
 * No indexing at the switch-operators
 * No cut functionality
 * No garbage collection has been implemented.
 * A subset of the ISO-Prolog is supported and compiled. No build-in predicates
   are supported, neither user-defined operators.

[1]: http://wambook.sourceforge.net/
[2]: http://www.swi-prolog.org/
[3]: http://www.dcc.fc.up.pt/~vsc/Yap/
[4]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.39.873
