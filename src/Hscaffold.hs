-- module: Hscaffold
-- author: Pedro Tacla Yamada
-- synopsis: Very simple file/directory structure scaffolding writer monad EDSL
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Hscaffold
    (
    -- * Running Hscaffold
      runHscaffold
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
    , ScaffoldActionType(..)
    , ScaffoldAction
    , ScaffoldActionV

    -- * Re-exports
    , module Data.Text
    , module Control.Monad.IO.Class
    , module Control.Monad.Writer
    , module System.Directory
    , module System.FilePath
    )
  where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Text              (Text)
import qualified Data.Text
import qualified Data.Text.IO           as Text
import           System.Directory
-- TODO - Disable this on Windows
import           System.FilePath
import           System.Posix.Files


-- | Run the scaffolding writer on the IO monad with no extensions
--
-- @
-- runHscaffold "." $ do
--     file "./.gitignore" (Text.unlines [ ".stack-work"
--                                       , "stuff"
--                                       , "here"
--                                       ])
--     directory "./src" $ do
--         file "./Main.hs" "main = putStrLn \\"Hello World\\""
--         file "./Other.hs" "other = putStrLn \\"Hello You\\""
-- @
runHscaffold :: FilePath -> WriterT ScaffoldActionV IO a -> IO a
runHscaffold root w = do
    (o, ws) <- runWriterT w
    mapM_ (runAction root) ws
    return o

-- | Run a single scaffolding action on the IO monad with no extensions
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
runAction root (Copy fp1 fp2) = do
    let fp1' = makeAbsolute fp1
        fp2' = makeAbsolute fp2
    copyFile fp1' fp2'
  where
    makeAbsolute fp = if isAbsolute fp then fp else root </> fp

-- | Accumulator for actions
type ScaffoldAction e = [ScaffoldActionType e]

-- | Accumulator for actions set with void extension
type ScaffoldActionV = ScaffoldAction ()

-- | Type of actions scaffolding can make, 'ScaffoldActionTypeExtension' is open
-- for extension through other data-types
data ScaffoldActionType e
    = File FilePath Text
    | Link FilePath FilePath
    | Directory FilePath (ScaffoldAction e)
    | SetPermissions Permissions FilePath
    | Copy FilePath FilePath
    | ScaffoldActionTypeExtension e
  deriving(Show, Eq, Ord)

-- | Create a directory with the nested contents
directory
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> WriterT (ScaffoldAction e) m b
    -> m b
directory fp nested = do
    (x, nested') <- runWriterT nested
    tell [Directory fp nested']
    return x

-- | Create a directory with the nested contents and permissions
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

-- | Create a file with the given contents
file
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> Text
    -> m ()
file fp txt = tell [File fp txt]

-- | Create a file with the given contents and permissions
fileWith
    :: MonadWriter (ScaffoldAction e) m
    => Permissions
    -> FilePath
    -> Text
    -> m ()
fileWith perms fp txt = do
    file fp txt
    tell [SetPermissions perms fp]

-- | Set permissions on a filepath
permissions
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> Permissions
    -> m ()
permissions fp perms = tell [SetPermissions perms fp]

-- | Create a symbolic link between two filepaths
link
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> FilePath
    -> m ()
link fp1 fp2 = tell [Link fp1 fp2]

-- | Write the empty string to a file
touch
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> m ()
touch fp = file fp ""

-- | Write the empty string to a file with given permissions
touchWith
    :: MonadWriter (ScaffoldAction e) m
    => Permissions
    -> FilePath
    -> m ()
touchWith perms fp = fileWith perms fp ""

-- | Copy a file from A to B
--
-- Non-absolute paths are treated relative to the *current* root, nested blocks
-- change the root
copy
    :: MonadWriter (ScaffoldAction e) m
    => FilePath
    -> FilePath
    -> m ()
copy fp1 fp2 = tell [Copy fp1 fp2]

-- | Copy a file from A to B and set permissions on B, see 'copy'
copyWith
    :: MonadWriter (ScaffoldAction e) m
    => Permissions
    -> FilePath
    -> FilePath
    -> m ()
copyWith perms fp1 fp2 = do
    copy fp1 fp2
    permissions fp2 perms
