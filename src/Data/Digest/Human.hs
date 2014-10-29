module Data.Digest.Human where

import           Data.Bits
import           Data.Bits.Extras
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C8
import           Data.List                     (genericIndex, foldl')
import           Data.Random.Normal            (mkNormals)
import qualified Data.Text              as T
import           Data.UUID.V4
import qualified Data.Vector.Bit        as BV
import qualified Data.Vector.Unboxed    as V
import           Data.Word

import           Data.Digest.Human.WordList

import Debug.Trace
-- | Build MxN normal matrix, transposed to make it simpler to multiply by a 1xM row vector
type Matrix a = [V.Vector a]
type Rows = Int
type Cols = Int
normMatMxN :: Rows -> Cols -> Matrix Double
normMatMxN m n = take m $ map (V.fromListN n . mkNormals) [0..]

-- | Dot a 1xM row vector by an MxN matrix
-- v *! M
(*!) :: (V.Unbox a, Num a) => V.Vector a -> Matrix a -> V.Vector a
(*!) v m = V.fromList $ (V.foldl1 (+)) `fmap` (V.map $ uncurry (*)) `fmap` (V.zip v) `fmap` m

-- | Compute a locality sensitive hash on the string
-- Expects string to be equal to the number of rows of the matrix
lsh :: Matrix Double -> B.ByteString -> Word64
lsh m s = complement $ BV.pack $ V.reverse $ V.map (<0) (v *! m)
  where
    v = V.map (fromIntegral . toInteger) $ V.fromList $ B.unpack $ B.take (length m) s

-- | Build a list of words taken from the given word list by chunking up
-- the given list of bytes into multiples of n, where 2^n >= length words
-- and then treating each chunk as a vector multiplied by a norm matrix
-- using LSH. The result is taken mod the length of the word list, and 
-- used as a lookup.
--
-- Example:
--
-- If the given word list had a length of 1661, this would be covered
-- by the indexes in the range (2^11 `mod` 1661), so we can consume the
-- input bytes in chunks of 11 bytes. The
toHumanWithWords :: [T.Text] -> [Word8] -> [T.Text]
toHumanWithWords words = map (genericIndex words . hash . B.pack) . chunksOf chunkBytes
  where
    hash s     = mod (lsh matrix s) $ w64 dimension
    dimension  = length words
    matrix     = normMatMxN chunkBytes dimension
    chunkBytes = case log2 $ w32 dimension of
        n | dimension < 2^n -> n
          | otherwise       -> n + 1

-- | Build a list of words using a default word list
toHuman :: [Word8] -> [T.Text]
toHuman = toHumanWithWords wordList

-- | Build a list of words from a ByteString
fromByteString :: B.ByteString -> [T.Text]
fromByteString = toHuman . B.unpack

-- | Build a list of words from a String
fromString :: String -> [T.Text]
fromString = fromByteString . C8.pack

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where
    go s = case splitAt k s of
                ([],b) -> []
                (a,b)  -> a : go b
