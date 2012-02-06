module WSC.Util where

import Control.Applicative
import Data.ByteString.UTF8 as UTF8
import Data.HashSet as HashSet

tupleA :: Applicative f => f a -> f b -> f (a, b)
tupleA = liftA2 (,)

set :: [String] -> HashSet ByteString
set = HashSet.fromList . fmap UTF8.fromString