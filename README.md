
Text.Pretty.Simple
==================

[![Build Status](https://secure.travis-ci.org/cdepillabout/pretty-simple.svg)](http://travis-ci.org/cdepillabout/pretty-simple)
[![Hackage](https://img.shields.io/hackage/v/pretty-simple.svg)](https://hackage.haskell.org/package/pretty-simple)
[![Stackage LTS](http://stackage.org/package/pretty-simple/badge/lts)](http://stackage.org/lts/package/pretty-simple)
[![Stackage Nightly](http://stackage.org/package/pretty-simple/badge/nightly)](http://stackage.org/nightly/package/pretty-simple)
![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)

`pretty-simple` is a pretty printer for Haskell data types that have a `Show`
instance.

For example, imagine the following Haskell data types and values:

```haskell
data Foo = Foo { foo1 :: Integer , foo2 :: [String] } deriving Show

foo :: Foo
foo = Foo 3 ["hello", "goodbye"]

data Bar = Bar { bar1 :: Double , bar2 :: [Foo] } deriving Show

bar :: Bar
bar = Bar 10.55 [foo, foo]
```

If you run this in `ghci` and type `print bar`, you'll get output like this:

```haskell
Bar {bar1 = 10, bar2 = [Foo {foo1 = 3, foo2 = ["hello","goodbye"], foo3 = 3.3},Foo {foo1 = 3, foo2 = ["hello","goodbye"], foo3 = 3.3}], bar3 = 10.55}
```

This is pretty hard to read.  Imagine if there were more fields or it were even
more deeply nested.  It would be even more difficult to read.

`pretty-simple` can be used to print `bar` in an easy-to-read format:

![example screenshot](/img/pretty-simple-example-screenshot.png?raw=true "example screenshot")

## Usage

`pretty-simple` can be easily used from `ghci` when debugging.

When using `stack` to run `ghci`, just append append the `--package` flag to
the command line to load `pretty-simple`.

```sh
$ stack ghci --package pretty-simple
```

Once you get a prompt in `ghci`, you can use `import` to get `pretty-simple`'s
`pPrint` function in scope.

```haskell
> import Text.Pretty.Simple (pPrint)
```

You can test out `pPrint` with simple data types like `Maybe` or tuples.

```haskell
> pPrint $ Just ("hello", "goodbye")
Just
    ( "hello"
    , "goodbye"
    )
```

## Features

`pretty-simple` has these features:

- Easy-to-read
    - Complex data types are simple to understand.
- Color
    - Prints in color using ANSI escape codes.
    - It is possible to print without color by using the `pPrintNoColor`
      function.
- Rainbox Parentheses
    - Easy to understand deeply nested data types.
- Configurable Indentation
    - Amount of indentation is configurable with the `pPrintOpt` function.
- Fast
    - No problem with data types thousands of lines long.
- Works with any data type with a `Show` instance
    - Some common Haskell data types have a `Show` instance that produces
      non-valid Haskell code.  `pretty-simple` will pretty-print even these
      data types.

## Why not `(some other package)`?

Other pretty-printing packages have some combination of these defects:

- No options for printing in color.
- No options for changing the amount of indentation
- Requires every data type to be an instance of some special typeclass (instead
  of just `Show`).
- Requires all `Show` instances to output valid Haskell code.

## Contributions

Feel free to open an issue or PR for any bugs/problems/suggestions/improvements.
