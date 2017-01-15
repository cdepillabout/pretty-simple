
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
data Foo = Foo
  { foo1 :: Integer
  , foo2 :: [String]
  } deriving (Data, Eq, Read, Show, Typeable)

data Bar = Bar
  { bar1 :: Double
  , bar2 :: [Foo]
  } deriving (Data, Eq, Read, Show, Typeable)

foo :: Foo
foo = Foo 3 ["hello", "goodbye"]

bar :: Bar
bar = Bar 10.55 [foo, foo]
```

If you run this in `ghci` and type `print bar`, you'll get an output like this:

```haskell
Bar {bar1 = 10, bar2 = [Foo {foo1 = 3, foo2 = ["hello","goodbye"], foo3 = 3.3},Foo {foo1 = 3, foo2 = ["hello","goodbye"], foo3 = 3.3}], bar3 = 10.55}
```

This is pretty hard to read.  Imagine if there were more fields or it were even
more deeply nested.  It would be even more difficult to read.

`pretty-simple` can be used to print `bar` in an easy-to-read format:

![example screenshot](/img/pretty-simple-example-screenshot.png?raw=true "example screenshot")

## Usage

TODO
