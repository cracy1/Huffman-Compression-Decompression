{-# LANGUAGE Safe #-} -- For automatic marking to work.
module Assessed1Part1 where

import Data.Char


{- Huffman Codes -}

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, x) = decodeAux tree tree x






-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (Leaf c _) _ [] = [c]
decodeAux _ _ [] = []
decodeAux (Leaf c _) originalTree xs = c : decodeAux originalTree originalTree xs
decodeAux (Branch left right _) originalTree (Z : xs) = decodeAux left originalTree xs
decodeAux (Branch left right _) originalTree (I : xs)  = decodeAux right originalTree xs

{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.
   
   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress x = decode(read tree, toBits bits) where (tree, bits) = extractTreeBits x


extractLength :: String -> Int -> (Int, String)
extractLength x acc | isDigit(head x) = extractLength (tail x) (acc * 10 + digitToInt (head x))
extractLength x acc = (acc, x)

extractTreeBits :: String -> (String, String)
extractTreeBits x = (take l s, drop l s)
                    where (l, s) = (fst (extractLength x 0), snd (extractLength x 0))

toBits :: String -> [Bit]
toBits [] = []
toBits (x : xs) | x == '0' = Z : toBits xs
toBits (_ : xs) = I : toBits xs

pulaincur tree = read tree

{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*' : xs) = xs
decompress' xs = decompress xs 


{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

--Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate x = removeDup [ (letter, counter) | letter <- x, let counter = (length.filter (== letter)) x, counter > 0] []

removeDup :: Eq a => [a] -> [a] -> [a]
removeDup [] acc = acc
removeDup (x : xs) acc | x `elem` acc = removeDup xs acc
removeDup (x : xs) acc = removeDup xs (x : acc)

