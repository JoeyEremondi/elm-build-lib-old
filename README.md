elm-build-lib
=============

A Haskell library wrapper around the Elm executable, to build files from within Haskell.

For more information on Elm, see http://elm-lang.org.

## Usage

There are two main steps to using this library: converting Elm source to a Module structure,
then compiling various modules.

To compile a string to a module, simply do

    let auxModule = moduleFromString (pack "Aux") (pack $ "module Aux where\n" ++ "x = 3")
    
or

    let mainModule = moduleFromString (pack "Main") (pack $ "import Aux\n" ++ "main = plainText (show Aux.x)")

Note that the first argument must match the name given in the `module X where`
declaration in your elm file.
Both arguments must be Text, not String.
You can use `moduleFromFile` similarly.

Once you have some modules, you can compile them into JavaScript or HTML:

    Right js <- buildModulesWithOptions defaultOptions mainModule [auxModule]

The first argument is always the module containing the `main` definition for Elm.
The list is the list of all files which are dependencies of the main module.
Files are written to a temp directory, then compiled using the `--make` option.

A current limitation is that only single-directory structures are supported.