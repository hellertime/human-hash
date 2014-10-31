module Data.Digest.Human.Hash (humanHashBy, hashMod) where

import           Data.Foldable
import           Data.Hashable
import 		 Data.List 	      (genericIndex)
import 		 Data.Vector   	      (Vector, (!))
import           Data.Sequence        (Seq)
import qualified Data.Sequence 	 as S
import           Data.Set             (Set, elemAt)
import 		 GHC.Exts             (IsList)
import qualified GHC.Exts        as G

-- | The class of indexable things.
class Indexable t where
  index      :: (Integral b) => t a -> b -> a
  indexByInt :: t a -> Int -> a

  -- ^ Most implemented indexable things tend to take an Int
  --   so default to providing a way to define an 'Indexable'
  --   via this 'Int' based index operation.
  index = flip $ flip indexByInt . fromIntegral

instance Indexable [] where
  index      = genericIndex
  indexByInt = (!!)

instance Indexable Vector where
  indexByInt = (!)

instance Indexable Seq where
  indexByInt = S.index

instance Indexable Set where
  indexByInt = flip elemAt

hashMod :: (Integral a, Hashable b) => a -> b -> [a]
hashMod r h = (flip mod r . fromIntegral . hash . (,) h) `fmap` [hash h..]

-- | Split the input 't a' into 'c' chunks of like size, and for each
-- chunk, hash and apply 'f'. Return the collected results.
--
-- Example:
--
-- > let words = ["foo", "bar"] in humanHashBy (\i -> (words !!).(`mod` 2)) 2 "test"
-- ["bar", "bar"]
--
humanHashBy :: (Foldable t, Hashable a) => (Int -> Int -> b) -> Int -> t a -> [b]
humanHashBy _ 0 = const []
humanHashBy f c = humanHash' . toList
  where
    humanHash' s = go 0 (length s) s
    go i k []    = []
    go i k s     = 
        case splitAt (k `quot` c) s of
            ([],b)  -> (f i . hash) b : []
            (a,b)   -> if length b < c
                           then (f i . hash) (a ++ b) : []
                           else (f i . hash) a : go (i + 1) k b
