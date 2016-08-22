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

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.List
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           System.Directory
-- TODO - Disable this on Windows
import           System.FilePath
import           System.Posix.Files
import           Text.Regex

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
runHscaffold :: MonadIO m => FilePath -> WriterT ScaffoldActionV m a -> m a
runHscaffold root w = do
    (o, ws) <- runWriterT w
    liftIO $ mapM_ (runAction root) ws
    return o

runHscaffoldIO :: FilePath -> ScaffoldMonadIO a -> IO a
runHscaffoldIO = runHscaffold

-- | Run the scaffolding writer and return an @.hsfiles@ 'Text' to use with
-- @stack-templates@
toHsfiles :: Writer ScaffoldActionV a -> Text
toHsfiles w =
    let (_, ws) = runWriter w
    in Text.stripEnd $ Text.unlines $
       concatMap (actionToHsfile ".") ws

-- | Converts hsfiles to hscaffold actions
fromHsfiles :: String -> ScaffoldAction e
fromHsfiles h =
    let (_, r) = foldr helper ("", []) matches
    in r
  where
    helper (Just (fp:_), _) (lacc, current) =
        ( ""
        , File fp lacc:current
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

-- | Converts a directory to scaffold actions
hscaffoldFromDirectory :: FilePath -> IO (ScaffoldAction e)
hscaffoldFromDirectory =
    hscaffoldFromDirectoryWith (filter (not . ("." `isPrefixOf`)))

-- | Converts a directory to scaffold actions with a custom filter function. By
-- default we ignore directories starting with @.@
hscaffoldFromDirectoryWith
    :: ([FilePath] -> [FilePath]) -> FilePath -> IO (ScaffoldAction e)
hscaffoldFromDirectoryWith p root = do
    ls <- p <$> getDirectoryContents root
    concat <$> mapM classify ls
  where
    fromFile fp = do
        txt <- Text.readFile fp
        return [ File fp txt ]
    classify fp = do
        let fp' = root </> fp
        isfl <- doesFileExist fp'
        isdir <- doesDirectoryExist fp'
        if isfl
            then fromFile fp'
            else if isdir then hscaffoldFromDirectoryWith p fp' else return []

hscaffoldToHaskell :: Foldable t => t (ScaffoldActionType e) -> Text
hscaffoldToHaskell hs = Text.stripEnd $
    Text.unlines $
        concatMap hscaffoldActionToHaskell hs

hscaffoldActionToHaskell :: ScaffoldActionType e -> [Text]
hscaffoldActionToHaskell h = case h of
    (File fp txt) ->
        [ "file " <> Text.pack (show fp <> " " <> show txt) ]
    (Directory fp nested) ->
        let nestedCode = concatMap hscaffoldActionToHaskell nested
            inestedCode = map ("    " <>) nestedCode
        in [ "directory " <> Text.pack (show fp) <> " $ do"
           ] <> inestedCode
    (Link fp1 fp2) ->
        [ "link " <> Text.pack (show fp1) <> " "
          <> Text.pack (show fp2)
        ]
    (SetPermissions p fp) ->
        [ "permissions " <> Text.pack (show fp)
            <> " (" <> Text.pack (show p) <> ")"
        ]
    (Copy fp1 fp2) ->
        [ "copy " <> Text.pack (show fp1)
            <> " " <> Text.pack (show fp2)
        ]
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

readHsfiles :: FilePath -> IO (ScaffoldAction e)
readHsfiles fp = fromHsfiles <$> readFile fp

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

-- | Run a single scaffolding action on the IO monad with no extensions
runAction :: FilePath -> ScaffoldActionType e -> IO ()
runAction _ (ScaffoldActionTypeExtension _) = return ()
runAction root (SetPermissions perms fp) =
    setPermissions (mkActionPath root fp) perms
runAction root (Link fp1 fp2) =
    createSymbolicLink (mkActionPath root fp1) (mkActionPath root fp2)
runAction root (File fp txt) =
    Text.writeFile (mkActionPath root fp) txt
runAction root (Directory fp nested) = do
    createDirectoryIfMissing True (mkActionPath root fp)
    mapM_ (runAction (mkActionPath root fp)) nested
runAction root (Copy fp1 fp2) = do
    let fp1' = mkActionPath root fp1
        fp2' = mkActionPath root fp2
    copyFile fp1' fp2'

-- | Very simple helper, if the second argument is absolute, returns it,
-- otherwise, return it relative to the first argument
mkActionPath
    :: FilePath
    -- ^ The root
    -> FilePath
    -- ^ A filepath
    -> FilePath
mkActionPath root fp = if isAbsolute fp then fp else root </> fp

-- | The writer monad transformer for scaffold actions
type ScaffoldMonadT m a = WriterT ScaffoldActionV m a

-- | The writer monad for scaffold actions, running in IO
type ScaffoldMonadIO a = WriterT ScaffoldActionV IO a

-- | The writer monad transformer for scaffold actions with an extension
type ScaffoldMonadET e m a = WriterT (ScaffoldAction e) m a

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
