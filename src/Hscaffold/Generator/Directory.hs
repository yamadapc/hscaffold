module Hscaffold.Generator.Directory where

import           Data.List
import qualified Data.Text.IO     as Text
import           System.Directory
import           System.FilePath

import           Hscaffold.Types

-- | Converts a directory to scaffold actions
hscaffoldFromDirectory :: FilePath -> IO (ScaffoldAction e)
hscaffoldFromDirectory =
    hscaffoldFromDirectoryWith (filter (not . ("." `isPrefixOf`)))

-- | Converts a directory to scaffold actions with a custom filter function. By
-- default we ignore directories starting with @.@
hscaffoldFromDirectoryWith :: ([FilePath] -> [FilePath])
                           -> FilePath
                           -> IO (ScaffoldAction e)
hscaffoldFromDirectoryWith p root = do
    ls <- p <$> getDirectoryContents root
    concat <$> mapM classify ls
  where
    fromFile fp fp' = do
        txt <- Text.readFile fp'
        return [ File fp txt ]
    fromDir fp fp' = do
        children <- hscaffoldFromDirectoryWith p fp'
        return [ Directory fp children ]
    classify fp = do
        let fp' = root </> fp
        isfl <- doesFileExist fp'
        isdir <- doesDirectoryExist fp'
        if isfl
            then fromFile fp fp'
            else if isdir then fromDir fp fp' else return []
