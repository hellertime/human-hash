module Data.Digest.Human where

import           Data.Bits
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.List                     (genericIndex, foldl')
import qualified Data.Text              as T
import           Data.UUID.V4
import           Data.Word

import           Data.Digest.Human.WordList

humanize :: Int -> C8.ByteString -> [T.Text]
humanize num s = [wordList `genericIndex` byte | byte <- compress (B16.encode s) num]

compress :: C8.ByteString -> Int -> [Word16]
compress s n = map (foldl' xor 0) w16s
  where
    w16s  = map (map $ go . C8.unpack) segs 
    go (a:b:[]) = (shiftL (w16 a) 8) .|. (w16 b)
    w16 = toEnum . fromEnum
    segs  = chunksOf (length bytes `quot` n) bytes
    bytes = chunksOfB 2 s

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where
    go s = case splitAt k s of
                ([],b) -> []
                (a,b)  -> a : go b

chunksOfB :: Int -> C8.ByteString -> [C8.ByteString]
chunksOfB k = go
  where
    go s = case C8.splitAt k s of
                (a,b) | C8.null a    -> []
                      | otherwise -> a : go b
