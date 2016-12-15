.PHONY: build build-haddock clean ghci haddock haddock-server lint repl test watch watch-tests watch-test
all: build

clean:
	stack clean

build: 
	stack build

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
