

all: configure build


build:
	cabal build

configure:
	cabal configure

clean:
	cabal clean

compiletests:
	cd tests
	make
