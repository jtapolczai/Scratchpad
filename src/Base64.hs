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

   decodeBase64Fragments,
   dec
   ) where

import Data.Word
import Data.Bits (shift, (.|.), (.&.))
import Data.Char (ord, chr)
import Control.Monad ((>=>))
import Data.List
import Data.Maybe
import Data.String.Utils
import Control.Monad.Loops

type Base64 = String
type Byte = Word8
type Decoder = Char -> Either String Byte
type Encoder = Byte -> Char
data BlockConverter a b = BC Int
                             [a] -> Maybe String
                             [a] -> Either String [b]
                             [a] -> Either String [b]

blockConvert :: BlockConverter a b -> [a] -> Either String [b]
blockConvert (BC size endErr f fEnd) xs =
   do let chk = chunksOf size xs
          err = endErr $ last chk
      initRes <- mapM f (init chk)
      lastRes <- if isJust err then Left $! fromJust err
                               else fEnd (last chk)
      return $! initres ++ [lastRes]

convert :: (Enum a, Enum b) => [a] -> [b]
convert = map (toEnum . fromEnum)

decodeTextURL :: String -> Either String String
decodeTextURL = decode64 toByteURL >=> (return . convert)

encodeTextURL :: String -> Base64
encodeTextURL = encode64 fromByteURL . convert

decode64 :: Decoder -> String -> Either String [Byte]
decode64 decoder = blockConvert (4,filler, decode')
   where 
      filler xs =
         if length (nub $ dropWhile ('='/=) xs) > 1 then Just "'=' character occurred before the end of the base64 string!"
         else Nothing

      decode' :: String -> Either String [Byte]
      decode' xs =
         do conv <- mapM decoder xs

            let b1 = shift (head conv) 2 .|. shift (conv !! 1) (-4)
                b2 = shift (conv !! 1) 4 .|. shift (conv !! 2) (-2)
                b3 = shift (conv !! 2) 6 .|.       (conv !! 3)
                block = take (length $ takeWhile ('='/=) xs) [b1,b2,b3]

            return $! block

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
toByte c | c `between` ('A','Z') = Right $ fromIntegral $ ord c - ord 'A'
         | c `between` ('a','z') = Right $ fromIntegral $ 26 + ord c - ord 'a'
         | c `between` ('0','9') = Right $ fromIntegral $ 52 + ord c - ord '0'
         | c == '+'        = Right 62
         | c == '/'        = Right 63
         | otherwise       = Left $ "Illegal character '" ++ c : "'!"

toByteURL :: Decoder
toByteURL c | c `between` ('A','Z') = Right $ fromIntegral $ ord c - ord 'A'
            | c `between` ('a','z') = Right $ fromIntegral $ 26 + ord c - ord 'a'
            | c `between` ('0','9') = Right $ fromIntegral $ 52 + ord c - ord '0'
            | c == '-'        = Right 62
            | c == '_'        = Right 63
            | otherwise       = Left $ "Illegal character '" ++ c : "'!"

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

-- |Finds all strings 
decodeBase64Fragments :: String -> [String]
decodeBase64Fragments =
   map (\(Right x) -> x)
   . filter isRight
   . map (decodeTextURL . takeWhile isBase64)
   . filter (startswith http)
   . tails

   where http = encodeTextURL "http:/"
         isBase64 '=' = True
         isBase64 x = isRight $ toByteURL x

isRight (Right _) = True
isRight _ = False

-- |State-of-the-art decoder.
dec :: IO String
dec = iterateWhile (/="exit") body
   where
      body = do putStrLn "Enter line (\"exit\" to quit): "
                line <- getLine
                putStrLn "Found links: "
                mapM_ putStrLn
                 $ decodeBase64Fragments
                 $ replace "theed" ""
                 $ replace "theedoa" ""
                 $ replace "Xthee" "L3" line
                return line
