module Data.Digest.Human.More where

import           Data.Bits.Extras                (log2, w32)
import           Data.Foldable
import           Data.Hashable
import qualified Data.Text                  as T
import           Data.Text                       (Text)
import           Data.Digest.Human.WordList

humanHashWith :: (Foldable t, Hashable a) => (Int -> Text) -> t a -> [Text]
humanHashWith f = humanHash' . toList
  where
    humanHash' s = go (length s) s
    go k s       = go' (chunkBy k) k s
    chunkBy k    = 
        case log2 $ w32 k of
            n | k < 2^n   -> n
              | otherwise -> n + 1
    go' c k s    =
        case splitAt c s of
            ([],b) -> []
            (a,b)  -> (f . (flip mod k) . hash) a : go' c k b

humanHash :: (Foldable t, Hashable a) => t a -> [Text]
humanHash = humanHashWith (wordList !!)
