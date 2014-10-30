module Data.Digest.Human (humanHash) where

import Data.Foldable
import Data.Hashable
import Data.Text     (Text)

import Data.Digest.Human.BsAtron

-- | Alias for 'humanHashBS'
humanHash :: (Foldable t, Hashable a) => Int -> t a -> [Text]
humanHash = humanHashBS
