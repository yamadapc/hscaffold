{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Hscaffold.EDSL
  where

import           Control.Monad.Writer
import           Data.Text            (Text)
import           System.Directory

import           Hscaffold.Types

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
