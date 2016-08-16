-- |Normalizes NTFS filenames that, while conformant with the NTFS
--  specification, cause problems for the Windows explorer.
--
--  Linux permits multiple files with names that are different only w.r.t.
--  case, and filesnames which start of end with whitespace. These are
--  technically legal, but the Windows explorer can't handle them, leaving
--  one with magically inaccessible and un-renameable files under Windows.
--
--  This module only works under Linux.
module NTFSCleaner where

import Data.Char
import Data.List
import qualified Data.UUID.V4 as U
import qualified Data.UUID as U
import System.Directory
import System.FilePath

-- |Recursively cleans a directory.
cleanDirectory :: Bool  -- Dummy-run? If true, no renamings actually take place, only messages are printed.
               -> FilePath
               -> IO ()
cleanDirectory dummy dir = do
   contents <- filter (not . flip elem [".",".."]) <$> getDirectoryContents dir
   (files, dirs) <- partitionM (doesFileExist . (dir </>)) contents
   let bucket f = groupBy (\x y -> f x == f y)
                  . sortBy (\x y -> compare (f x) (f y))
       filesB = bucket normalize files
       dirsB = bucket normalizeD dirs

   -- Normalize files and directories.
   mapM_ (cleanEntries dummy dir) filesB
   mapM_ (cleanEntries dummy dir) dirsB

   -- |Get the new directory names and recurse on them.
   contents' <- filter (not . flip elem [".",".."]) <$> getDirectoryContents dir
   (_, dirs') <- partitionM (doesFileExist . (dir </>)) contents'

   mapM_ (cleanDirectory dummy . (dir </>)) dirs'

-- |Strips leading/trailing whitespace and converts the filename to lowercase.
--  @normalize = map toLower . normalize'@
normalize :: FilePath -> FilePath
normalize = map toLower . normalize'

-- |Strips leading/trailing whitespace from a filename and removes
--  all whitespace from its extension.
normalize' :: FilePath -> FilePath
normalize' x = strip (takeBaseName x) <.> filter (not . isSpace) (takeExtension x)

-- |Like 'normalize'', but for directories. Does not try to take the extension of
--  of the filepath (since directories have none).
normalizeD' :: FilePath -> FilePath
normalizeD' x = strip $ takeBaseName x <.> takeExtension x

-- |Like 'normalize', but for directories. See 'normalizeD''.
normalizeD :: FilePath -> FilePath
normalizeD = map toLower . normalizeD'

-- |Removes leading/trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p l = sel l ([],[])
  where
    sel [] a = return a
    sel (x:xs) (ts,fs) = do
        r <- p x
        sel xs $ if r then (x:ts,fs) else (ts,x:fs)

-- |Takes a directory and a list of children and normalizes them.
--
--  Leading/trailing whitespace will be stripped. If the list of children has
--  a length of > 1, a number i of the format "x_i.ext" will be appended to each child x
--  with extension ext.
--
--  If, for any file, its proposed renaming already exists, we append a GUID V4 g,
--  resulting in the format: "x_i_g.ext"
cleanEntries :: Bool -- Dummy-run? If true, no renamings actually take place, only messages are printed.
             -> FilePath
             -> [FilePath]
             -> IO ()
cleanEntries dummy dir fps = renamings >>= mapM_ doRename 
   where
      rFile = if dummy then renameFileDummy else renameFileLog
      rDir = if dummy then renameDirectoryDummy else renameDirectoryLog

      renamings = 
         (if length fps <= 1 then id
          else zipWith addNum [1..])
         <$> mapM (\x -> do
                      isFile <- doesFileExist (dir </> x)
                      return $ (x, (if isFile then normalize' else normalizeD') x))
                  fps

      addNum i (x,x') = (x, appendName x' ("_" ++ show i))

      doRename (x,x') =
         if x == x' then return ()
         else do
            isFile <- doesFileExist (dir </> x)
            isDir <- doesDirectoryExist (dir </> x)

            targetExists <- (||) <$> doesFileExist (dir </> x') <*> doesDirectoryExist (dir </> x')
            guid <- U.nextRandom

            let x'' = if targetExists then appendName x' ("_" ++ U.toString guid)
                      else x'

            if isFile then rFile (dir </> x) (dir </> x'')
            else if isDir then rDir (dir </> x) (dir </> x'')
            else error $ "cleanEntries: " ++ (dir </> x) ++ " does not exist!"

-- |Appends the second argument to the base name of the first, leaving the
--  extension unchanged.
appendName :: FilePath -> String -> FilePath
appendName x y = (takeBaseName x ++ y) <.> takeExtension x

-- |A dummy implementation for 'renameFile'. Does nothing except printing
--  a message on stdout.
renameFileDummy :: FilePath -> FilePath -> IO ()
renameFileDummy from to = putStrLn $ "renameFile: '" ++ from ++ "' -> " ++ to

-- |A dummy implementation for 'renameDirectory'. Does nothing except printing
--  a message on stdout.
renameDirectoryDummy :: FilePath -> FilePath -> IO ()
renameDirectoryDummy from to = putStrLn $ "renameDirectory: '" ++ from ++ "' -> " ++ to

-- |A logging implementation for 'renameFile'. In addition to renaming, it
--  writes a message to stdout.
renameFileLog :: FilePath -> FilePath -> IO ()
renameFileLog from to = do 
   putStrLn $ "renameFile: '" ++ from ++ "' -> " ++ to
   renameFile from to

-- |A logging implementation for 'renameDirectory'. In addition to renaming, it
--  writes a message to stdout.
renameDirectoryLog :: FilePath -> FilePath -> IO ()
renameDirectoryLog from to = do 
   putStrLn $ "renameDirectory: '" ++ from ++ "' -> " ++ to
   renameDirectory from to
