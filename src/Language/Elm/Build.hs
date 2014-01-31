{-# LANGUAGE OverloadedStrings #-}

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


type ModuleName = Text

type ModuleSource = Text

newtype InternalModule = Module (ModuleName, ModuleSource)

type Module = InternalModule

type Javascript = Text

-- | Abstraction for the options given to the elm executable
-- Note that not all Elm options may be avaliable
data BuildOptions = BuildOptions {
    elmBinPath :: Maybe String,
    elmRuntimePath :: Maybe String,
    makeHtml :: Bool
  }

-- Default options are: `elm` as binary, no runtime given, and generate JS only
defaultOptions :: BuildOptions
defaultOptions = BuildOptions {
 elmBinPath = Nothing,
 elmRuntimePath = Nothing,
 makeHtml = False
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
  mapM_ writeElmSource otherModules
  writeElmSource mainModule

  let binPath = fromMaybe "elm" $ elmBinPath options
  let runtimeOption = maybe Nothing (\path -> Just $ "--runtime=" ++ path) $ elmRuntimePath options
  let genJSOption = if (makeHtml options) then Nothing else (Just "--only-js")
  let resultExt = if (makeHtml options) then ".html" else ".js"
  
  let cmdlineOptions = ["--make",  "--build-dir=" ++ dir, "--cache-dir=" ++ dir ++"/cache"] ++ catMaybes [runtimeOption, genJSOption] ++ [unpack mainName ++ ".elm"]
  
  (exitCode, stdout, stderr) <- readProcessWithExitCode binPath cmdlineOptions []
  
  case exitCode of
       ExitFailure i -> return $ Left $ "Elm failed with exit code " ++ (show i) ++ " and errors:" ++ stdout ++ stderr
       _ -> do
          putStrLn $ stdout ++ stderr
          let outputPath = dir ++ "/" ++ unpack mainName ++ resultExt
          exists <- doesFileExist outputPath
          case exists of
               False -> return $ Left "Could not find output file from Elm"
               _ -> do
                  retJS <- TextIO.readFile outputPath
                  return $ Right retJS
    
  )
  where
    writeElmSource (Module (moduleName, source)) = TextIO.writeFile (unpack moduleName ++ ".elm") source