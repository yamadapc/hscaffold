{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hscaffold
    ( runWriter
    , runHscaffold
    , runAction

    , directory
    , file
    , link
    , Permissions(..)
    , fileWith
    , directoryWith
    , permissions

    , ScaffoldActionType(..)
    , ScaffoldAction
    , ScaffoldActionV
    )
  where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Text                (Text)
import qualified Data.Text.IO as Text
import           System.Directory
-- TODO - Disable this on Windows
import           System.Posix.Files
import           System.FilePath

data ScaffoldActionType e = File FilePath Text
                          | Link FilePath FilePath
                          | Directory FilePath (ScaffoldAction e)
                          | SetPermissions Permissions FilePath
                          | ScaffoldActionTypeExtension e
  deriving(Show, Eq, Ord)

type ScaffoldAction e = [ScaffoldActionType e]
type ScaffoldActionV = ScaffoldAction ()

directory
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> WriterT (ScaffoldAction e) m b
    -> m b
directory fp nested = do
    (x, nested') <- runWriterT nested
    tell [Directory fp nested']
    return x

directoryWith
    :: MonadWriter (ScaffoldAction e) m
    => Permissions
    -> FilePath
    -> WriterT (ScaffoldAction e) m b
    -> m b
directoryWith perms fp nested = do
    x <- directory fp nested
    tell [SetPermissions perms fp]
    return x

file
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> Text
    -> m ()
file fp txt = tell [File fp txt]

fileWith
    :: MonadWriter (ScaffoldAction e) m
    => Permissions
    -> FilePath
    -> Text
    -> m ()
fileWith perms fp txt = do
    file fp txt
    tell [SetPermissions perms fp]

permissions
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> Permissions
    -> m ()
permissions fp perms = tell [SetPermissions perms fp]

link
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> FilePath
    -> m ()
link fp1 fp2 = tell [Link fp1 fp2]

runHscaffold :: FilePath -> WriterT ScaffoldActionV IO a -> IO a
runHscaffold root w = do
    (o, ws) <- runWriterT w
    mapM_ (runAction root) ws
    return o

runAction :: FilePath -> ScaffoldActionType () -> IO ()
runAction root (SetPermissions perms fp) =
    setPermissions fp perms
runAction root (Link fp1 fp2) =
    createSymbolicLink fp1 fp2
runAction root (File fp txt) =
    Text.writeFile (root </> fp) txt
runAction root (Directory fp nested) = do
    createDirectoryIfMissing True (root </> fp)
    mapM_ (runAction (root </> fp)) nested
