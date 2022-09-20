
## 4.1.1.0

*   Make the pretty-printed output with `outputOptionsCompact` enabled a little
    more compact.
    [#110](https://github.com/cdepillabout/pretty-simple/pull/110).
    Thanks [@juhp](https://github.com/juhp)!
*   Add a `--compact` / `-C` flag to the `pretty-simple` executable that enables
    `outputOptionsCompact`.
    [#111](https://github.com/cdepillabout/pretty-simple/pull/111).
    Thanks again @juhp!
*   Add `pTraceWith` and `pTraceShowWith` to `Debug.Pretty.Simple`.
    [#104](https://github.com/cdepillabout/pretty-simple/pull/104).
    Thanks [@LeviButcher](https://github.com/LeviButcher)!

## 4.1.0.0

*   Fix a regression which arose in 4.0, whereby excess spaces would be inserted for unusual strings like dates and IP addresses.
    [#105](https://github.com/cdepillabout/pretty-simple/pull/105)
*   Attach warnings to debugging functions, so that they're easy to find and remove.
    [#103](https://github.com/cdepillabout/pretty-simple/pull/103)
*   Some minor improvements to the CLI tool:
    *   Add a `--version`/`-v` flag.
        [#83](https://github.com/cdepillabout/pretty-simple/pull/83)
    *   Add a trailing newline.
        [#87](https://github.com/cdepillabout/pretty-simple/pull/87)
    *   Install by default, without requiring a flag.
        [#94](https://github.com/cdepillabout/pretty-simple/pull/94)

## 4.0.0.0

*   Expand `OutputOptions`:
    *   Compactness, including grouping of parentheses.
        [#72](https://github.com/cdepillabout/pretty-simple/pull/72)
    *   Page width, affecting when lines are grouped if compact output is enabled.
        [#72](https://github.com/cdepillabout/pretty-simple/pull/72)
    *   Indent whole expression. Useful when using `pretty-simple` for one part
        of a larger output.
        [#71](https://github.com/cdepillabout/pretty-simple/pull/71)
    *   Use `Style` type for easier configuration of colour, boldness etc.
        [#73](https://github.com/cdepillabout/pretty-simple/pull/73)
*   Significant internal rewrite of printing code, to make use of the [prettyprinter](https://hackage.haskell.org/package/prettyprinter)
    library. The internal function `layoutString` can be used to integrate with
    other `prettyprinter` backends, such as [prettyprinter-lucid](https://hackage.haskell.org/package/prettyprinter-lucid)
    for HTML output.
    [#67](https://github.com/cdepillabout/pretty-simple/pull/67)

## 3.3.0.0

*   Add an output option to print escaped and non-printable characters
    literally when outputting strings.
    [#68](https://github.com/cdepillabout/pretty-simple/pull/68) and
    [#69](https://github.com/cdepillabout/pretty-simple/pull/69)
    Thanks Joe Hermaszewski ([@expipiplus1](https://github.com/expipiplus1))!

## 3.2.3.0

*   Fix a bug that messes up printing identifiers with `'` in the name.
    Now identifiers like `data Don't = Don't` show up properly.
    [#65](https://github.com/cdepillabout/pretty-simple/pull/65)
    Thanks George Thomas ([@georgefst](https://github.com/georgefst))!

## 3.2.2.0

*   Remove whitespace from the ends of lines.
    [#62](https://github.com/cdepillabout/pretty-simple/pull/62)
    Thanks Gaith Hallak ([@ghallak](https://github.com/ghallak))!

## 3.2.1.0

*   Added `pTraceOpt` functions to `Debug.Pretty.Simple`.
    [#58](https://github.com/cdepillabout/pretty-simple/pull/58)
    Thanks again [sureyeaah](https://github.com/sureyeaah)!

## 3.2.0.0

*   Add support for pretty-printing Haskell character literals.
    [#57](https://github.com/cdepillabout/pretty-simple/pull/57)
    Thanks again [sjakobi](https://github.com/sjakobi)!

## 3.1.1.0

*   Added a `pPrintString` function for pretty-printing a `String` that is the
    output of `show`.  Implemented in
    [#54](https://github.com/cdepillabout/pretty-simple/pull/54). Thanks
    [sureyeaah](https://github.com/sureyeaah)!
*   Fix build on GHC-7.10.3.
    [#55](https://github.com/cdepillabout/pretty-simple/pull/55).  Thanks
    [sjakobi](https://github.com/sjakobi).

## 3.1.0.0

*   Numbers are now highlighted in green by default.  Implemented in
    [#51](https://github.com/cdepillabout/pretty-simple/pull/51).
    Thanks [lawrencebell](https://github.com/lawrencebell)!

## 3.0.0.0

*   pretty-simple now escapes non-printable characters by default.  A field
    called `outputOptionsEscapeNonPrintable` has been added to `OutputOptions`
    to control this behavior.  Implemented in
    [#44](https://github.com/cdepillabout/pretty-simple/pull/44). Thanks
    [dminuoso](https://github.com/dminuoso)!
*   pretty-simple now checks the output `Handle` to determine whether to print
    in color when using functions like `pPrint`.  This makes it so that you
    can redirect output to a file on disk and still be able to read the
    output from `pPrint`!  Implemented in
    [#47](https://github.com/cdepillabout/pretty-simple/pull/47).  Thanks
    [metiulekm](https://github.com/metiulekm)!
*   Add functions like `pHPrint` for specifying the `Handle` to output to.
    Added in [#47](https://github.com/cdepillabout/pretty-simple/pull/47).

## 2.2.0.1
*   Fixed a [bug](https://github.com/cdepillabout/pretty-simple/pull/41) where
    the parser failed to parse escaped quotation marks in string literals.
    Thanks [Andreas](https://github.com/anka-213)!


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
