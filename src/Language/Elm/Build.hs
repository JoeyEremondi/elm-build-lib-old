module Language.Elm.Build where

import System.Cmd
import System.Directory
import qualified System.IO.Temp as Temp



-- |Wrapper type for an Elm module
newtype Module = Module {moduleToString :: String}

type Javascript = String


-- | Write the given Elm module strings to a temp directory
-- and compile them using Elm, returning the resulting JavaScript
buildModules :: [(String, Module)] -> String ->  IO Javascript
buildModules fileModuleList mainModule = Temp.withTempDirectory "" ".elm_temp" (\dir -> do
  mapM_ writeElmSource fileModuleList
  cd <- getCurrentDirectory
  setCurrentDirectory dir

  _exitCode <- system $ "elm --make --only-js " ++ mainModule ++ ".elm"
  
  let outputPath = dir ++ "/build/" ++ mainModule
  
  retJS <- readFile outputPath
  
  setCurrentDirectory cd
  
  return retJS
  
  )
  where
    writeElmSource (path, Module source) = writeFile path source