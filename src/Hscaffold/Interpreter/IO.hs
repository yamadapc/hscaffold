{-# LANGUAGE FlexibleContexts #-}
module Hscaffold.Interpreter.IO
  where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import qualified Data.Text.IO                 as Text
import           System.Directory
import           System.IO.Temp
-- TODO - Disable this on Windows
import           System.Posix.Files

import           Hscaffold.Interpreter.Common
import           Hscaffold.Types

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

withTemporaryHscaffold :: (MonadMask m, MonadIO m) => ScaffoldMonadT m a -> m b -> m b
withTemporaryHscaffold =
    withTemporaryHscaffold' "hscaffold"

withTemporaryHscaffold' :: (MonadMask m, MonadIO m)
                        => String
                        -> ScaffoldMonadT m a
                        -> m b
                        -> m b
withTemporaryHscaffold' name scaffold action =
    withSystemTempDirectory name $
        \tmp -> do
            runHscaffold tmp scaffold
            action

withTemporaryHscaffoldIO :: ScaffoldMonadIO a -> IO b -> IO b
withTemporaryHscaffoldIO = withTemporaryHscaffold

withTemporaryHscaffoldIO' :: String -> ScaffoldMonadIO a -> IO b -> IO b
withTemporaryHscaffoldIO' = withTemporaryHscaffold'
