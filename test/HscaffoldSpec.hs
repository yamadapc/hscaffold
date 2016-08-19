{-# LANGUAGE OverloadedStrings #-}
module HscaffoldSpec
  where

import           Control.Exception
import           Control.Monad
import           Hscaffold
import           System.Directory

import           Test.Hspec

spec = do
    describe "the writers" $ do
        it "create proper instructions for our results" $ do
            let (_, ws) = runWriter $ do
                          file "stuff-bang.hs" ""
                          directory "./something" $ do
                              file "stuff-here.hs" ""
                              file "stuff-there.hs" ""
            ws `shouldBe` [ File "stuff-bang.hs" ""
                          , Directory "./something" [ File "stuff-here.hs" ""
                                                    , File "stuff-there.hs" ""
                                                    ]
                          ]

    describe "the runner" $ do
        describe "runAction" $ do
            it "creates files" $ do
                void $ (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void $ (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (File "./tmp" "stuff")
                ecnts <- try $ readFile "./tmp" :: IO (Either SomeException String)
                void $ (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                let Right cnts = ecnts
                cnts `shouldBe` "stuff"

            it "creates directories" $ do
                void $ (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void $ (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (Directory "./tmp/" [])
                t <- doesDirectoryExist "./tmp"
                void $ (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                t `shouldBe` True

            it "creates directory contents" $ do
                void $ (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                void $ (try $ removeFile "./tmp" :: IO (Either SomeException ()))
                runAction "." (Directory "./tmp/" [File "stuff" "here"])
                ecnts <- try $ readFile "./tmp/stuff" :: IO (Either SomeException String)
                void $ (try $ removeDirectoryRecursive "./tmp" :: IO (Either SomeException ()))
                let Right cnts = ecnts
                cnts `shouldBe` "here"
