module GpoM.Types.Policy where

import Data.Default
import System.FilePath.Posix

class Default a => Policy a where
  -- | Relative path where a `Policy` template will be exposed
  path :: a -> FilePath
  template :: a
  template = def
