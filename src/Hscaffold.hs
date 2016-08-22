-- module: Hscaffold
-- author: Pedro Tacla Yamada
-- synopsis: Very simple file/directory structure scaffolding writer monad EDSL
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Hscaffold
    (
    -- * Running Hscaffold
      runHscaffold
    -- * Convert Hscaffold to HSFILES (@stack templates@)
    , toHsfiles
    , writeToHsfiles
    -- * Convert HSFILES to Hscaffold
    , fromHsfiles
    , fromHsfilesW
    , readHsfiles

    -- * Convert a directory to Hscaffold
    , hscaffoldFromDirectory
    -- * Compile Hscaffold to Haskell code
    , hscaffoldToHaskell

    -- ** Finer grained runners
    , runAction
    , runWriter
    , runWriterT

    -- * EDSL Combinators
    , directory
    , file
    , link
    , copy
    , touch

    -- ** Setting permissions
    , permissions
    , fileWith
    , directoryWith
    , copyWith
    , touchWith
    , Permissions(..)

    -- * Types
    , ScaffoldMonadT
    , ScaffoldMonadIO
    , ScaffoldActionType(..)
    , ScaffoldAction
    , ScaffoldActionV
    , ScaffoldMonadET

    -- * Utilities
    , withTemporaryHscaffold
    , withTemporaryHscaffold'
    , withTemporaryHscaffoldIO
    , withTemporaryHscaffoldIO'

    -- * Helpers
    , runHscaffoldIO
    , mkActionPath

    -- * Re-exports
    , Text
    , module Control.Monad.IO.Class
    , module Control.Monad.Writer
    , module System.Directory
    , module System.FilePath
    )
  where

import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Text                     (Text)
import           System.Directory
-- TODO - Disable this on Windows
import           System.FilePath

import           Hscaffold.EDSL
import           Hscaffold.Generator.Directory
import           Hscaffold.Generator.Hsfiles
import           Hscaffold.Interpreter.Common
import           Hscaffold.Interpreter.Haskell
import           Hscaffold.Interpreter.Hsfiles
import           Hscaffold.Interpreter.IO
import           Hscaffold.Types
