{-# LANGUAGE OverloadedStrings #-}

module Hscaffold.Interpreter.Haskell where

import           Control.Monad.Writer
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Hscaffold.Types

-- | Generates Haskell code from scaffold actions
hscaffoldToHaskell :: Foldable t => t (ScaffoldActionType e) -> Text
hscaffoldToHaskell hs = Text.stripEnd $
    Text.unlines $
        concatMap hscaffoldActionToHaskell hs

hscaffoldActionToHaskell :: ScaffoldActionType e -> [Text]
hscaffoldActionToHaskell h =
    case h of
        (File fp txt) -> [ "file " <> Text.pack (show fp <> " " <> show txt) ]
        (Directory fp nested) ->
            let nestedCode = concatMap hscaffoldActionToHaskell nested
                inestedCode = map ("    " <>) nestedCode
            in
                [ "directory " <> Text.pack (show fp) <> " $ do" ] <>
                    inestedCode
        (Link fp1 fp2) -> [ "link " <> Text.pack (show fp1) <> " "
                              <> Text.pack (show fp2)
                          ]
        (SetPermissions p fp) ->
            [ "permissions " <> Text.pack (show fp)
                <> " (" <>
                Text.pack (show p) <>
                ")"
            ]
        (Copy fp1 fp2) -> [ "copy " <> Text.pack (show fp1)
                              <> " " <>
                              Text.pack (show fp2)
                          ]
        _ -> []
