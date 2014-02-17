{-# LANGUAGE OverloadedStrings #-}

{-|
A Haskell library wrapper around the Elm executable, to build files from within Haskell.

For more information on Elm, see http://elm-lang.org.

There are two main steps to using this library: converting Elm source to a Module structure,
then compiling various modules.

To compile a string to a module, simply do

    > let auxModule = moduleFromString (pack "Aux") (pack $ "module Aux where\n" ++ "x = 3")
    
or

    > let mainModule = moduleFromString (pack "Main") (pack $ "import Aux\n" ++ "main = plainText (show Aux.x)")

Note that the first argument must match the name given in the @module X where@
declaration in your elm file.
Both arguments must be Text, not String.
You can use `moduleFromFile` similarly.

Once you have some modules, you can compile them into JavaScript or HTML:

    > Right js <- buildModulesWithOptions defaultOptions mainModule [auxModule]

The first argument is always the module containing the @main@ definition for Elm.
The list is the list of all files which are dependencies of the main module.
Files are written to a temp directory, then compiled using the @--make@ option.

A current limitation is that only single-directory structures are supported.

-}


module Language.Elm.Build (
  Module,
  Javascript,
  BuildOptions(..),
  ModuleName,
  ModuleSource,
  defaultOptions,
  moduleFromString,
  moduleFromFile,
  buildModules,
  buildModulesWithOptions
  
  ) where

import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import Data.Maybe (catMaybes, fromMaybe)
import System.IO.Temp (withTempDirectory)
import Data.Text
import qualified Data.Text.IO as TextIO





-- | Synonym for module names (i.e. Data.Text, Main, etc.)
type ModuleName = Text

-- | Type for module source code
type ModuleSource = Text

-- | Wrapper for modules, which have a name and source code
newtype InternalModule = Module (ModuleName, ModuleSource)

-- | Opaque type representing an Elm module loaded from a string or file
type Module = InternalModule

-- | Type representing Javascript output (as a string)
type Javascript = Text

-- | Abstraction for the options given to the elm executable
-- Note that not all Elm options may be avaliable
data BuildOptions = BuildOptions {
    elmBinPath :: Maybe String,
    elmRuntimePath :: Maybe String,
    makeHtml :: Bool,
    dependenciesFile :: Maybe String
  }

-- |Default options are: `elm` as binary, no runtime given, and generate JS only
defaultOptions :: BuildOptions
defaultOptions = BuildOptions {
 elmBinPath = Nothing,
 elmRuntimePath = Nothing,
 makeHtml = False,
 dependenciesFile = Nothing
}

-- | Generate a module with the given Module name (e.g. 'MyLib.Foo')
-- and the given source code
moduleFromString :: ModuleName -> ModuleSource -> Module
moduleFromString name source = Module (name, source)

-- | Read a module from a file, with the given module name and file path
moduleFromFile :: ModuleName -> FilePath -> IO Module
moduleFromFile name path = do
  src <- TextIO.readFile path
  return $ moduleFromString name src

-- | Build a group of elm modules with the `elm` from the system `$PATH`
-- generating JavaScript using the default runtime location
buildModules ::  Module -> [Module]  ->  IO (Either String Javascript)
buildModules = buildModulesWithOptions defaultOptions
  
-- | Given an elm "main" module, and a list of other modules,
-- compile them using the `--make` option and the given options
buildModulesWithOptions :: BuildOptions -> Module -> [Module]  -> IO (Either String Javascript)
buildModulesWithOptions options mainModule@(Module (mainName, _)) otherModules = withTempDirectory "" ".elm_temp" (\dir -> do
  --Write Elm dependencies file, if it exists
  case (dependenciesFile options) of
    Just depPath -> do
      depString <- readFile depPath
      writeFile (dir ++ "/elm_dependencies.json") depString
    Nothing -> return ()
  
  mapM_ (writeElmSource dir) otherModules
  writeElmSource dir mainModule

  let binPath = fromMaybe "elm" $ elmBinPath options
  let runtimeOption = maybe Nothing (\path -> Just $ "--runtime=" ++ path) $ elmRuntimePath options
  let genJSOption = if (makeHtml options) then Nothing else (Just "--only-js")
  let resultExt = if (makeHtml options) then ".html" else ".js"
  
  let cmdlineOptions = ["--make",  "--build-dir=" ++ dir ++ "/build", "--cache-dir=" ++ dir ++"/cache", "--src-dir=" ++ dir] ++ catMaybes [runtimeOption, genJSOption] ++ [ unpack mainName ++ ".elm"]
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode binPath cmdlineOptions []
  
  case exitCode of
       ExitFailure i -> return $ Left $ "Elm failed with exit code " ++ (show i) ++ " and errors:" ++ stdout ++ stderr
       _ -> do
          let outputPath = dir ++ "/build/" ++ unpack mainName ++ resultExt
          exists <- doesFileExist outputPath
          case exists of
               False -> return $ Left "Could not find output file from Elm"
               _ -> do
                  retJS <- TextIO.readFile outputPath
                  return $ Right retJS
    
  )
  where
    writeElmSource dir (Module (moduleName, source)) = do
      let path = dir ++ "/" ++ unpack moduleName ++ ".elm"
      TextIO.writeFile path source
