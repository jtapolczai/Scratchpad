{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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


choose :: Int -> IO Int
choose = randomRIO . (0,)

chooseItem :: M.Map Int a -> IO (Maybe a)
chooseItem m = flip M.lookup m <$> choose (M.size m - 1)

readPasswordFile :: String -> IO (M.Map Int String)
readPasswordFile f = (M.fromList . zip [0..] . lines <$> readFile f) `catch` h
   where
      h :: IOException -> IO (M.Map Int String)
      h _ = hPutStrLn stderr ("Can't open password file " ++ f ++ "!") >> return M.empty

generatePassword :: Int -> M.Map Int String -> IO String
generatePassword num = return . concat . map capitalise . catMaybes <=< sequence . replicate num . chooseItem

capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs

generatePasswords :: Int -> Int -> M.Map Int String -> IO [String]
generatePasswords numPass numWords = sequence . replicate numPass . generatePassword numWords

main :: IO ()
main = readPasswordFile cPASSWORD_FILE >>= generatePasswords cNUM_PASSWORDS cNUM_WORDS >>= mapM_ putStrLn
