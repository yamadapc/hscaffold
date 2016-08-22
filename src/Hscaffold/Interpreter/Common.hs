module Hscaffold.Interpreter.Common where

import           System.FilePath

-- | Very simple helper, if the second argument is absolute, returns it,
-- otherwise, return it relative to the first argument
mkActionPath :: FilePath
             ->
             -- ^ The root
             FilePath
             ->
             -- ^ A filepath
             FilePath
mkActionPath root fp = if isAbsolute fp then fp else root </> fp
