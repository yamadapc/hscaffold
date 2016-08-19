{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hscaffold
    ( runWriter
    , runHscaffold
    , runAction

    , directory
    , file
    , ScaffoldActionType(..)
    , ScaffoldAction
    , ScaffoldActionM(..)
    )
  where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Text                (Text)
import qualified Data.Text.IO as Text
import           System.Directory
import           System.FilePath

data ScaffoldActionType = File FilePath Text
                        | Directory FilePath ScaffoldAction
  deriving(Show, Eq, Ord)

type ScaffoldAction = [ScaffoldActionType]

newtype ScaffoldActionM a = ScaffoldActionM (Writer ScaffoldAction a)
  deriving(Functor, Applicative, Monad)

instance Monoid (ScaffoldActionM ()) where
    mempty = pure ()
    mappend = liftA2 mappend

directory
    :: MonadWriter [ScaffoldActionType] m
    => FilePath
    -> WriterT ScaffoldAction m b
    -> m b
directory fp nested = do
    (x, nested') <- runWriterT nested
    tell [Directory fp nested']
    return x

file
    :: MonadWriter [ScaffoldActionType] m
    => FilePath
    -> Text
    -> m ()
file fp txt = tell [File fp txt]

runHscaffold root w = do
    let (o, ws) = runWriter w
    mapM_ (runAction root) ws
    return o

runAction root (File fp txt) =
    Text.writeFile (root </> fp) txt
runAction root (Directory fp nested) = do
    createDirectoryIfMissing True (root </> fp)
    mapM_ (runAction (root </> fp)) nested
