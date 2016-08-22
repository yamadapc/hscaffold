{-# LANGUAGE OverloadedStrings #-}
module Hscaffold.Interpreter.Hsfiles
  where

import           Control.Monad.Writer
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text

import           Hscaffold.Interpreter.Common
import           Hscaffold.Types

-- | Run the scaffolding writer and return an @.hsfiles@ 'Text' to use with
-- @stack-templates@
toHsfiles :: Writer ScaffoldActionV a -> Text
toHsfiles w =
    let (_, ws) = runWriter w
    in Text.stripEnd $ Text.unlines $
       concatMap (actionToHsfile ".") ws

-- | Convert a single scaffolding action to hsfiles
--
-- Ignores everything but the 'file' directives
actionToHsfile :: FilePath -> ScaffoldActionType e -> [Text]
actionToHsfile root a = case a of
    (File fp txt) ->
        [ "{-# START_FILE " <> Text.pack (mkActionPath root fp) <> " #-}" ] <>
        Text.lines txt
    (Directory fp nested) -> concatMap (actionToHsfile (mkActionPath root fp)) nested
    _ -> []

-- | Shortcut for
--
-- @
-- writeToHsfiles = do h <- toHsfiles w; liftIO $ Text.writeFile fp h
-- @
writeToHsfiles :: MonadIO m => FilePath -> Writer ScaffoldActionV a -> m ()
writeToHsfiles fp w = do
    let h = toHsfiles w
    liftIO $ Text.writeFile fp h
