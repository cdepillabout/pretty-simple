## 2.2.0.1
*   Fixed a [bug](https://github.com/cdepillabout/pretty-simple/pull/41) where the parser failed to parse escaped quotation marks in string literals. Thanks [Andreas](https://github.com/anka-213)!


## 2.2.0.0

*   Fixed a [bug](https://github.com/cdepillabout/pretty-simple/pull/33) with a
    missing space after strings.  Thanks again
    [Andrew](https://github.com/andrew-lei)!
*   Add a command line flag `--color` to be able to set whether to use colors for
    a dark background (`--color dark-bg`), a light background (`--color light-bg`),
    or no color (`--color no-color`).  This is from
    [great work](https://github.com/cdepillabout/pretty-simple/pull/35) by
    [Andrew](https://github.com/andrew-lei)!
*   Made parsing/printing lazy - pretty-printing will now output strings continuously
    as they're read, handling potentially infinite input.

## 2.1.0.1

*   Fix a [bug](https://github.com/cdepillabout/pretty-simple/pull/32) where
    printing deeply nested data structures would take exponential time.  Thanks
    [Andrew](https://github.com/andrew-lei)!

## 2.1.0.0

*   Make strings have indentation by default when pretty-printed.  See
    [#26](https://github.com/cdepillabout/pretty-simple/pull/26).  Thanks
    [Milan](https://github.com/Wizek)!

## 2.0.2.1

*   Add a small command-line program that will pretty print anything from stdin
    called `pretty-print`.  It can be installed to `~/.local/bin` if you enable
    the flag `buildexe` like so:

    ```sh
    $ stack install pretty-simple-2.0.2.1 --flag pretty-simple:buildexe
    ```

    When you run it, you can paste something you want formatted on stdin, then
    press <key>Ctrl</key>-<key>D</key>.  It will print the formatted version on
    stdout:

    ```sh
    $ pretty-simple
    [(Just 3, Just 4)]

    ^D

    [
        ( Just 3
        , Just 4
        )
    ]
    ```

## 2.0.2.0

*   Fix a [problem](https://github.com/cdepillabout/pretty-simple/pull/20) with
    the pTraceShow functions not working correctly.

## 2.0.1.0

*   Added the `Debug.Pretty.Simple` that exports functions that work like
    `Debug.Trace`.
