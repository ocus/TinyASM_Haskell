.PHONY: all bench build clean configure haddock hpc install repl run test

all: install configure build haddock test bench

bench:
	cabal bench --jobs

build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests --enable-coverage

haddock:
	cabal haddock --hyperlink-source
	# dist/doc/html/${packageName}/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	# tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:TinyASM

run:
	cabal run --jobs TinyASM

test:
	rm -f *.tix
	cabal test --jobs --show-details=always
	cabal check

testvm:
	rm -f *.tix
	cabal test TestVM --jobs --show-details=always

testparser:
	rm -f *.tix
	cabal test TestParser --jobs --show-details=always
