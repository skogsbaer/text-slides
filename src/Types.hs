module Types where

import qualified Data.Text as T

type Fail a = Either T.Text a
