
## 2.0.2.1

*   Add a small command-line program that will pretty print anything from stdin
    called `pretty-print`.  It can be installed to `~/.local/bin` if you enable
    the flag `buildexe` like so:

    ```sh
    $ stack install pretty-simple-2.0.1.1 --flag pretty-simple:buildexe
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
