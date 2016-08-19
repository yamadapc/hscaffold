# hscaffold
Very simple file/directory structure scaffolding writer monad EDSL
```haskell
runHscaffold "." $ do
    file "./.gitignore" (Text.unlines [ ".stack-work"
                                      , "stuff"
                                      , "here"
                                      ])
    directory "./src" $ do
        file "./Main.hs" "main = putStrLn \"Hello World\""
        file "./Other.hs" "other = putStrLn \"Hello You\""
```

## License
MIT
