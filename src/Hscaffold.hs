{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hscaffold
    ( runHscaffold
    , runAction
    , runWriter
    , runWriterT

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


-- | Run the scaffolding writer on the IO monad with no extensions
--
--     runHscaffold "." $ do
--         file "./.gitignore" (Text.unlines [ ".stack-work"
--                                           , "stuff"
--                                           , "here"
--                                           ])
--         directory "./src" $ do
--             file "./Main.hs" "main = putStrLn \"Hello World\""
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

-- | Accumulator for actions
type ScaffoldAction e = [ScaffoldActionType e]

-- | Accumulator for actions set with void extension
type ScaffoldActionV = ScaffoldAction ()

-- | Type of actions scaffolding can make, 'ScaffoldActionTypeExtension' is open
-- for extension through other data-types
data ScaffoldActionType e = File FilePath Text
                          | Link FilePath FilePath
                          | Directory FilePath (ScaffoldAction e)
                          | SetPermissions Permissions FilePath
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
