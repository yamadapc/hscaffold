{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Hscaffold.Generator.Hsfiles
  where

import           Control.Arrow
import           Control.Monad.Writer
import qualified Data.Text                     as Text
import           System.FilePath
import           Text.Regex

import           Hscaffold.Types

-- | Converts hsfiles to hscaffold actions
fromHsfiles :: String -> ScaffoldAction e
fromHsfiles h =
    let (_, r) = foldr helper ("", []) matches
    in r
  where
    helper (Just (fp:_), _) (lacc, current) =
        ( ""
        , File (normalise fp) lacc:current
        )
    helper (_, l) (lacc, current) =
        ( Text.stripEnd (l <> "\n" <> lacc)
        , current
        )
    re = mkRegex "^{-# START_FILE (.+) #-}( +)?$"
    matches = map (matchRegex re &&& Text.pack) (lines h)

-- | Converts hsfiles to a hscaffold monad
fromHsfilesW :: MonadWriter (ScaffoldAction e) m => String -> m ()
fromHsfilesW h = tell (fromHsfiles h)

readHsfiles :: FilePath -> IO (ScaffoldAction e)
readHsfiles fp = fromHsfiles <$> readFile fp
