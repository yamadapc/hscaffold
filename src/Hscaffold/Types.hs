module Hscaffold.Types
    where

import           Control.Monad.Writer
import           Data.Text            (Text)
import           System.Directory

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
