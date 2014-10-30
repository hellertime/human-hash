module Data.Digest.Human
    (
      -- * Export the bsAtron based human hash
      humanHash

      -- * Export PGP word-list based human hash
    , humanHashPGP
    , humanHashPGP'
    )
  where

import Data.Foldable
import Data.Hashable
import Data.Text     (Text)

import Data.Digest.Human.BsAtron
import Data.Digest.Human.PGP

-- | Alias for 'humanHashBS'
humanHash :: (Foldable t, Hashable a) => Int -> t a -> [Text]
humanHash = humanHashBS
