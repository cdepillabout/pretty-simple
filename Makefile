.PHONY: bench benchmark build build-haddock clean dump-splices dump-th example example-profile ghci haddock haddock-server lint repl test upload watch watch-tests watch-test
all: build

bench: benchmark
benchmark:
	stack bench

build: 
	stack build

clean:
	stack clean

# dump the template haskell
dump-splices: dump-th
dump-th:
	-stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices" | sort

example:
	stack build --flag pretty-simple:buildexample
	stack exec pretty-simple-example

example-profile:
	stack build --flag pretty-simple:buildexample --profile
	stack exec pretty-simple-example -- +RTS -p

haddock: build-haddock
build-haddock:
	stack build --haddock

# Watch for changes.
watch:
	stack build --file-watch --fast .

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast .

# Run ghci using stack.
repl: ghci
ghci:
	stack ghci

test:
	stack test

# Run hlint.
lint:
	hlint src/

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

# Upload this package to hackage.
upload:
	stack upload .
