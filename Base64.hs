module Base64 (
   decode64,
   encode64,
   decodeTextURL,
   encodeTextURL,
   toByte,
   toByteURL,
   fromByte,
   fromByteURL,
   Base64,
   Byte,
   Decoder,
   Encoder,
   ) where

import Data.Word
import Data.Bits (shift, (.|.), (.&.))
import Data.Char (ord, chr)
import Control.Monad ((>=>))

type Base64 = String
type Byte = Word8
type Decoder = Char -> Maybe Byte
type Encoder = Byte -> Char

convert :: (Enum a, Enum b) => [a] -> [b]
convert = map (toEnum . fromEnum)

decodeTextURL :: String -> Maybe String
decodeTextURL = decode64 toByteURL >=> (return . convert)

encodeTextURL :: String -> Base64
encodeTextURL = encode64 fromByteURL . convert

decode64 :: Decoder -> String -> Maybe [Byte]
decode64 _ [] = return []
decode64 d cs@(_:_:'=':'=':[]) = decode' d 1 (take 2 cs) []
decode64 d cs@(_:_:_  :'=':[]) = decode' d 2 (take 3 cs) []
decode64 d cs@(_:_:_  :_  :[]) = decode' d 3 (take 4 cs) []
decode64 d cs@(_:_:_  :_  :re) = decode' d 3 (take 4 cs) re

decode' :: Decoder -> Int -> String -> String -> Maybe [Byte]
decode' decoder n cs re =
   do conv <- mapM decoder cs

          -- The three result bytes
      let b1 = shift (head conv) 2 .|. shift (conv !! 1) (-4)
          b2 = shift (conv !! 1) 4 .|. shift (conv !! 2) (-2)
          b3 = shift (conv !! 2) 6 .|.       (conv !! 3)

          block = take n [b1,b2,b3]

      rest <- decode64 decoder re
      return $! block ++ rest


encode64 :: Encoder-> [Byte] -> Base64
encode64 _ [] = []
encode64 e (b1:[])       = encode' e 2 [b1,0,0]    []
encode64 e (b1:b2:[])    = encode' e 3 [b1,b2,0] []
encode64 e (b1:b2:b3:re) = encode' e 4 [b1,b2,b3] re

encode' :: Encoder -> Int -> [Byte] -> [Byte] -> Base64
encode' encoder n (b1:b2:b3:[]) re = block ++ encode64 encoder re
   where
      c1 =           shift b1 (-2)
      c2 = 0x3F .&. (shift b1   4 .|. shift b2 (-4))
      c3 = 0x3F .&. (shift b2   2 .|. shift b3 (-6))
      c4 = 0x3F .&.  b3

      block = map encoder (take n [c1,c2,c3,c4]) ++ replicate (4-n) '='

toByte :: Decoder
toByte c | c `between` ('A','Z') = Just $ fromIntegral $ ord c - ord 'A'
         | c `between` ('a','z') = Just $ fromIntegral $ 26 + ord c - ord 'a'
         | c `between` ('0','9') = Just $ fromIntegral $ 52 + ord c - ord '0'
         | c == '+'        = Just 62
         | c == '/'        = Just 63
         | otherwise       = Nothing

toByteURL :: Decoder
toByteURL c | c `between` ('A','Z') = Just $ fromIntegral $ ord c - ord 'A'
            | c `between` ('a','z') = Just $ fromIntegral $ 26 + ord c - ord 'a'
            | c `between` ('0','9') = Just $ fromIntegral $ 52 + ord c - ord '0'
            | c == '-'        = Just 62
            | c == '_'        = Just 63
            | otherwise       = Nothing

fromByte :: Encoder
fromByte b | b `between` (0,25)  = chr $ ord 'A' + fromIntegral b
           | b `between` (26,51) = chr $ ord 'a' + fromIntegral (b-26)
           | b `between` (52,61) = chr $ ord '0' + fromIntegral (b-52)
           | b == 62       = '+'
           | b == 63       = '/'

fromByteURL :: Encoder
fromByteURL b | b `between` (0,25)  = chr $ ord 'A' + fromIntegral b
              | b `between` (26,51) = chr $ ord 'a' + fromIntegral (b-26)
              | b `between` (52,61) = chr $ ord '0' + fromIntegral (b-52)
              | b == 62       = '-'
              | b == 63       = '_'

between :: Ord a => a -> (a,a) -> Bool
between x (lower, upper) = lower <= x && x <= upper