{-# LANGUAGE OverloadedStrings #-}
module HscaffoldSpec
  where

import           Control.Exception
import           Control.Monad
import           Data.Maybe
import qualified Data.Text         as Text
import           Hscaffold
import           System.IO.Temp

import           Test.Hspec

directoryShouldExist dir = do
    e <- doesDirectoryExist dir
    e `shouldBe` True

fileShouldExist fp = do
    e <- doesFileExist fp
    e `shouldBe` True

fileShouldHaveContents fp txt = do
    e <- readFile fp
    e `shouldBe` txt

spec :: Spec
spec = do
    describe "the writers" $
        it "create proper instructions for our results" $ do
            let (_, ws) = runWriter $ do
                    file "stuff-bang.hs" ""
                    directory "./something" $ do
                        file "stuff-here.hs" ""
                        file "stuff-there.hs" ""
            (ws :: ScaffoldAction ())
                `shouldBe` [ File "stuff-bang.hs" ""
                           , Directory "./something"
                                       [ File "stuff-here.hs" ""
                                       , File "stuff-there.hs" ""
                                       ]
                           ]

    describe "runHscaffold" $
        it "works without surprises" $ do
            withSystemTempDirectory "hscaffold" $
                \tmp -> do
                    runHscaffold tmp $ do
                        file "stuff-bang.hs" ""
                        directory "./something" $ do
                            file "stuff-here.hs" ""
                            file "stuff-there.hs" ""
                    directoryShouldExist (tmp </> "something")
                    fileShouldExist (tmp </> "stuff-bang.hs")
                    fileShouldExist (tmp </> "something" </> "stuff-here.hs")
                    fileShouldExist (tmp </> "something" </> "stuff-there.hs")

    describe "toHsfiles" $ do
        it "works without surprises" $
            let h = toHsfiles $ do
                    file "stuff-bang.hs" "stuff here"
                    file "other/stuff-bang.hs" "stuff here"
                    directory "other" $ do
                        file "other-stuff-bang.hs" "even more stuff here"
            in h `shouldBe` Text.stripEnd
               (Text.unlines [ "{-# START_FILE ./stuff-bang.hs #-}"
                             , "stuff here"
                             , "{-# START_FILE ./other/stuff-bang.hs #-}"
                             , "stuff here"
                             , "{-# START_FILE ./other/other-stuff-bang.hs #-}"
                             , "even more stuff here"
                             ])

    describe "fromHsfiles" $ do
        it "works without surprises" $
            let h = (init
                     (unlines
                         [ "{-# START_FILE ./stuff-bang.hs #-}"
                         , "stuff here"
                         , "{-# START_FILE ./other/stuff-bang.hs #-}"
                         , "stuff here"
                         , "{-# START_FILE ./other/other-stuff-bang.hs #-}"
                         , "even more stuff here"
                         ]))
            in (fromHsfiles h :: ScaffoldActionV) `shouldBe`
               [ File "./stuff-bang.hs" "stuff here"
               , File "./other/stuff-bang.hs" "stuff here"
               , File "./other/other-stuff-bang.hs" "even more stuff here"
               ]

        it "works without surprises" $
            let h = (init
                     (unlines
                         [ "{-# START_FILE ./stuff-bang.hs #-}"
                         , "stuff here"
                         , "multiline"
                         , " ya"
                         , "{-# START_FILE ./other/stuff-bang.hs #-}"
                         , "stuff here"
                         , ""
                         , "{-# START_FILE ./other/other-stuff-bang.hs #-}      "
                         , "even more stuff here"
                         , "  here too"
                         ]))
            in (fromHsfiles h :: ScaffoldActionV) `shouldBe`
               [ File "./stuff-bang.hs" "stuff here\nmultiline\n ya"
               , File "./other/stuff-bang.hs" "stuff here"
               , File "./other/other-stuff-bang.hs" "even more stuff here\n  here too"
               ]

    describe "hscaffoldToHaskell" $ do
        it "works without surprises" $
            let h = hscaffoldToHaskell $ execWriter $ do
                    file "stuff-bang.hs" "stuff here"
                    file "other/stuff-bang.hs" "stuff here"
                    directory "other" $ do
                        file "other-stuff-bang.hs" "even more stuff here"
            in h `shouldBe` Text.stripEnd
               (Text.unlines [ "file \"stuff-bang.hs\" \"stuff here\""
                             , "file \"other/stuff-bang.hs\" \"stuff here\""
                             , "directory \"other\" $ do"
                             , "    file \"other-stuff-bang.hs\" \"even more stuff here\""
                             ])

    describe "hscaffoldFromDirectory" $ do
        it "works without surprises" $ do
            h <- hscaffoldFromDirectory "./src" :: IO (ScaffoldActionV)
            let ms = mapMaybe (\x -> case x of File fp _ -> Just fp; _ -> Nothing) h
            ms `shouldContain` ["Hscaffold.hs"]

    describe "the runner" $
        describe "runAction" $ do
            it "creates files" $ do
                void (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (File "./tmp" "stuff")
                ecnts <- try $ readFile "./tmp" :: IO (Either SomeException String)
                void (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                let Right cnts = ecnts
                cnts `shouldBe` "stuff"

            it "creates directories" $ do
                void (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (Directory "./tmp/" [])
                t <- doesDirectoryExist "./tmp"
                void (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                t `shouldBe` True

            it "creates directory contents" $ do
                void (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (Directory "./tmp/" [ File "stuff" "here" ])
                ecnts <- try $ readFile "./tmp/stuff" :: IO (Either SomeException String)
                void (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                let Right cnts = ecnts
                cnts `shouldBe` "here"
