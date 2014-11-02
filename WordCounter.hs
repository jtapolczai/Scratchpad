module WordCounter where

import System.Directory
import System.IO
import Data.Maybe
import Control.Monad
import Data.List
import System.FilePath

data Tree a = Tree a [Tree a] deriving (Eq, Show)

collectLeaves :: Tree a -> [a]
collectLeaves (Tree a []) = [a]
collectLeaves (Tree _ xs) = concat $ map collectLeaves xs

getNumLines :: String -> IO Int
getNumLines = readFile >=> return . length . lines

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith suf xs = suf == (reverse $ take (length suf) $ reverse xs)

getFilesRec :: String -> IO (Tree String)
getFilesRec path = do contents <- getDirectoryContents path
                      let contents' = map (path </>)
                                      $ filter (not . flip elem [".",".."])
                                      $ contents

                      dirs <- filterM doesDirectoryExist contents'
                      files <- filterM doesFileExist contents'
                      subtrees <- mapM getFilesRec dirs

                      return $ Tree path $ subtrees ++ map (flip Tree []) files

main' :: String -> String -> IO ()
main' path ext =
   do files <- liftM collectLeaves $ getFilesRec path
      results <- mapM getNumLines $ filter (endsWith ext) files
      putStrLn $ "Number of lines: " ++ show (sum results)

main :: IO ()
main = do path <- putStr "Enter path: " >> hFlush stdout >> getLine
          ext <- putStr "Enter extension (with dot): " >> hFlush stdout >> getLine
          main' path ext
