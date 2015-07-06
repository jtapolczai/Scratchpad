{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |Generates passwords in the 'correct horse battery staple' method recommended by
--  issue 936 of XKCD.
module PasswordGenerator where

import Control.Exception
import Control.Monad
import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import System.IO
import System.Random

cNUM_PASSWORDS = 1
cNUM_WORDS = 3
cPASSWORD_FILE = "en.txt"


-- |Uniformly and randomly chooses a value between 0 and n (inclusive).
choose :: Int -> IO Int
choose = randomRIO . (0,)

-- |Randomly chooses a value of a map. The map's keys have start at 0 and be contiguous.
chooseItem :: M.Map Int a -> IO (Maybe a)
chooseItem m = flip M.lookup m <$> choose (M.size m - 1)

-- |Reads in a password file with the given name. Each line will be an entry.
--  The keys will be the line numbers. If the file can't be read, an error is printed
--  to stderr and an empty map is returned.
readPasswordFile :: String -> IO (M.Map Int String)
readPasswordFile f = (M.fromList . zip [0..] . lines <$> readFile f) `catch` h
   where
      h :: IOException -> IO (M.Map Int String)
      h _ = hPutStrLn stderr ("Can't open password file " ++ f ++ "!") >> return M.empty

-- |Randomly selects n elements from a map and concatenates them. Each word will be
--  capitalised.
generatePassword :: Int -> M.Map Int String -> IO String
generatePassword num = return . concat . map capitalise . catMaybes <=< sequence . replicate num . chooseItem

-- |Capitalises the first letter of a string.
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs

-- |Creates n passwords, each consisting of m words.
generatePasswords :: Int -> Int -> M.Map Int String -> IO [String]
generatePasswords numPass numWords = sequence . replicate numPass . generatePassword numWords

-- |Reads in the default password file and prints out a pre-defined number of passwords.
main :: IO ()
main = readPasswordFile cPASSWORD_FILE >>= generatePasswords cNUM_PASSWORDS cNUM_WORDS >>= mapM_ putStrLn
