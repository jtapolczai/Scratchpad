{-# LANGUAGE LambdaCase #-}

module XMLReplacer where

import Text.XML.Light hiding (findChild)
import Text.XML.Light.Cursor
import Data.Maybe
import Data.String.Utils (startswith)
import Data.List.Split (splitOn)
import Control.Monad
import System.Directory
import Prelude hiding (pred)

type Filename = String
type SearchedText = String
type ReplacementText = String
type Replacement = (SearchedText, ReplacementText)

-- |Goes through a list and applies a function f to every
--  element which fulfills a predicate pred.
--  At the end, the number of changed elements is returned.
traverse :: (a -> Bool) -> (a -> a) -> [a] -> ([a], Int)
traverse = traverse' 0
   where
      traverse' res _ _ [] = ([], res)
      traverse' res pred f (x:xs) =
         if pred x then (f x : xs', res' + 1) else (x:xs', res')
         where (xs', res') = traverse' res pred f xs

-- |Reads a 2-column CSV-file and returns the results.
readReplacements :: Filename -> IO (Maybe [Replacement])
readReplacements f = liftM (mapM parse . lines) (readFile f)
   where
      parse l = case splitOn ";" l of
                     (o:n:[])  -> Just (o,n)
                     _         -> Nothing

-- |Reads an XML file and returns its contents.
readXML :: Filename -> IO Cursor
readXML = readFile >=> (return . fromContent . head . parseXML)

-- |Replaces the initial part of a string, given by the old
--  value of the replacement, with the new value.
replace :: Replacement -> String -> String
replace (o,n) s = n ++ drop (length o) s

-- |Sets a given attribute of an element to a new value.
--  If the attribute does not yet exist, it is inserted.
setAttrib :: Element -> String -> String -> Element
setAttrib el att val = el{elAttribs = f $ elAttribs el}
   where
      f as = if numChanged > 0 then as'
             else newAtt : as
         where (as', numChanged) = traverse ((att ==) . qName . attrKey)
                                            (\a -> a{attrVal = val})
                                            as

               newAtt = Attr{attrKey = QName{qName=att,
                                             qURI=Nothing,
                                             qPrefix=Nothing},
                             attrVal = val}

-- |Finds the first replacement that fits for a given string.
findMatch :: [Replacement] -> String -> Maybe Replacement
findMatch re s = listToMaybe $ dropWhile f re
   where f (o,_) = not $ startswith o s

-- |Takes a <window> element, a list of replacements,
--  and replaces it according to the first match in the
--  replacement list.
doReplacement :: [Replacement] -> Element -> Element
doReplacement re el = el{elAttribs = replacedAttribs}
   where
      (replacedAttribs,_) = traverse (("path"==) . qName . attrKey)
                                     repl
                                     (elAttribs el)
      repl a = case findMatch re (attrVal a) of
                  Just r -> a{attrVal = replace r (attrVal a)}
                  Nothing -> a

-- |Lifts a function from element to element
--  to a partial one from content to content.
liftContent :: (Element -> Element) -> Content -> Content
liftContent f (Elem e) = Elem $ f e
liftContent _ (Text s) = error $ "Found Text node with content: {" ++ cdData s ++ "}"

-- |Takes a cursor placed at the root (<session>) and replaces the 
--  paths of all <windows>/<window> tags according to the list of the
--  given replacements.
--  If something goes wrong, the original cursor is returned.
performReplacements :: Cursor -> [Replacement] -> Cursor
performReplacements cur re = fromMaybe undefined (windows >>= mkRepl >>= parent >>= parent)

   where windows = findChild (hasName "windows") cur

         mkRepl = findChild (hasName "window")
                  >=> while (isJust . right')
                             (modify $ doReplacement re)
                             right'
         modify = modifyContent . liftContent
         right' = findRight (hasName "window")

while :: Monad m => (a -> Bool) -> (a -> a) -> (a -> m a) -> a -> m a
while pred f step v = if pred v then step (f v) >>= while pred f step
                      else return (f v)

-- |Returns True iff the current content is an element
--  and has the given name (case sensitive).
hasName :: String -> Cursor -> Bool
hasName s c = case current c of
                 (Elem el) -> (s==) $ qName $ elName el
                 _         -> False

-- |Reads a file containing replacements and an XML session file,
--  performs the given replacements, and writes the result into
--  the given output file.
replaceIO :: Filename -- ^The name of the file containing the replacement list.
          -> Filename -- ^The XML document to read.
          -> Filename -- ^The XML document to which to write.
          -> IO (Either String ()) -- ^The result of the operation. Left s with
                                   --  an error message s in case of failure,
                                   --  Right () in case of success.
replaceIO r xml out =
   do re <- readReplacements r
      case re of Nothing -> return $! Left "Couldn't read replacement file!"
                 Just re' -> do cur <- readXML xml
                                writeFile out (showContent
                                               $ toTree
                                               $ performReplacements cur re')
                                return $! Right ()


repairSessions :: Filename -> IO ()
repairSessions r = getDirectoryContents "."
                   >>= filterM doesFileExist
                   >>= mapM_ (\f -> replaceIO r f ("repaired/" ++ f) >> putStrLn (f ++ " finished!")) 