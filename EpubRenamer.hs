module EPUBRenamer where

import System.Directory
import System.IO
import Data.List
import Control.Monad.Error

import Data.Functor

import qualified Data.ByteString.Lazy as BL

import Codec.Epub
import Codec.Epub.Data.Metadata

import Control.Exception

-- |Takes a directory and renamed the epubs contained therein
--  to @[author] - [title].epub@
renameEpubs :: String -> IO ()
renameEpubs dir = do
   contents <- filter isEpub <$> getDirectoryContents dir
   renamings <- (mapM (getRenaming dir) contents) >>= printErrs
   --print' renamings
   putStrLn "Type 'Proceed' to proceed, anything else to abort."
   line <- getLine
   if line == "Proceed" then
      mapM_ (uncurry $ doRenaming dir) renamings
   else
      putStrLn "Doing nothing."


-- |Returns True iff the file ends in ".epub"
isEpub :: String -> Bool
isEpub xs = reverse ".epub" == take 5 (reverse xs)

doRenaming :: String -> String -> String -> IO ()
doRenaming dir oldF newF =
   catch (renameFile (dir ++ "/" ++ oldF) (dir ++ "/" ++ newF))
         handle
   where
      handle :: IOException -> IO ()
      handle = print

-- |Takes a filename of an epub, and returns
--  the renaming (old/new filename).
getRenaming :: String -> String -> IO (Either String (String, String))
getRenaming dir file = (runErrorT $ do
   xmlString <- getPkgXmlFromZip $ dir ++ "/" ++ file
   meta <- getMetadata xmlString
   let creator = intercalate ", " $ map creatorText $ metaCreators meta
       title = intercalate ", " $ map titleText $ metaTitles meta

   return (file, creator ++ " - " ++ title ++ ".epub"))

printErrs :: [Either String a] -> IO [a]
printErrs [] = return []
printErrs (Left x:xs) = putStrLn x >> printErrs xs
printErrs (Right x:xs) = (x:) <$> (printErrs xs)

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight called with Left!"
fromRight (Right r) = r

print' :: [(String, String)] -> IO ()
print' = mapM_ pr
   where
      pr (o,n) = putStrLn $ o ++ " ==> " ++ n ++ "\n"
