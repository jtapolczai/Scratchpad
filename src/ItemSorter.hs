module ItemSorter where

import Control.Arrow (right)
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import qualified Data.Time as T
import qualified Text.Parsec as P

-- Types
-------------------------------------------------------------------------------

type ItemName = String

type Day = Int
type Month = Int
type Year = Int
data Date = Date Year Month Day
   deriving (Show, Eq, Ord)

data Item = Item {
   _itemName :: ItemName,
   _itemContainerType :: ContainerType,
   _itemQuantity :: Int,
   _itemExpiration :: Date
}

data ContainerType = Bag | Bar | Can | Jar | Pack
   deriving (Show, Eq, Ord, Enum, Bounded)

-- Date
-------------------------------------------------------------------------------

-- |Creates a date out of a year, month, and day. Checks for validity.
--  The year is bounded in (1970,9999).
mkDate :: Year -> Month -> Day -> Maybe Date
mkDate y m d | isValidYear y
               && isValidMonth m
               && isValidDay y m d = Just $ Date y m d
             | otherwise = Nothing

-- |Creates a date from a possibly missing month or day.
--  The date is created with the latest possible values, e.g.
--  just passing 2012 would result in the date 2012.12.31.
mkVagueDate
   :: Year
   -> Maybe Month
   -> Maybe Day
   -> Maybe Date
mkVagueDate y (Just m) (Just d) = mkDate y m d
mkVagueDate y (Just m) Nothing = mkDate y m (daysInMonth y m)
mkVagueDate y Nothing Nothing = mkDate y 12 31

-- |Creates the latest possibly date (9999.12.31).
mkMaxDate :: Date
mkMaxDate = Date 9999 31 12

-- |Returns the current system date.
mkCurrentDate :: IO Date
mkCurrentDate = do
   (y,m,d) <- T.toGregorian . T.utctDay <$> T.getCurrentTime
   return $ Date (fromIntegral y) m d

isValidYear :: Year -> Bool
isValidYear y = y >= 1970 && y < 9999

isValidMonth :: Month -> Bool
isValidMonth m = m >= 1 && m <= 12

isValidDay :: Year -> Month -> Day -> Bool
isValidDay y m d = d >= 1 && d <= daysInMonth y m

daysInMonth :: Year -> Month -> Int
daysInMonth y m | m `elem` [1,3,5,7,8,10,12] = 31
                | m `elem` [4,6,9,11] = 30
                | m == 2 = if isLeapYear y then 29 else 28
                | otherwise = 0

isLeapYear y =
   (divBy y 400) || (divBy y 4 && not (divBy y 100))
   where
      divBy x y = x `mod` y == 0

-- Parsers
-------------------------------------------------------------------------------

readItem :: P.Parsec String () Item
readItem = do
   qty <- decimal
   P.char ';'
   contype <- readContainerType
   P.char ';'
   name <- P.many1 (P.noneOf ";")
   P.char ';'
   expiration <- readDate P.<|> readIndefinite
   P.endOfLine
   return $ Item name contype qty expiration

readContainerType :: P.Parsec String () ContainerType
readContainerType =
   P.try (P.string "bag" >> return Bag)
   P.<|> P.try (P.string "bar" >> return Bar)
   P.<|> (P.string "can" >> return Can)
   P.<|> (P.string "jar" >> return Jar)
   P.<|> (P.string "pack" >> return Pack)

readDate :: P.Parsec String () Date
readDate = do
   comp1 <- decimal
   comp2 <- P.try $ P.optionMaybe readComp
   comp3 <- P.try $ P.optionMaybe readComp
   let ret = case (comp2, comp3) of
         (Just m, Just y) -> mkDate y m comp1
         (Just y, Nothing) -> mkVagueDate y (Just comp1) Nothing
         (Nothing, Nothing) -> mkVagueDate comp1 Nothing Nothing
   maybe (P.unexpected "invalid date") return ret
   where
      readComp :: P.Parsec String () Int
      readComp = do P.char '.'
                    decimal

readIndefinite :: P.Parsec String () Date
readIndefinite = do
   P.string "indefinite"
   return mkMaxDate

decimal :: P.Parsec String () Int
decimal = read <$> P.many1 P.digit

readItems :: P.Parsec String () [Item]
readItems = P.many readItem

-- Main program
-------------------------------------------------------------------------------

-- |Reads an item list from a file and returns it, sorted by expiration date.
readItemList :: FilePath -> IO (Either P.ParseError [Item])
readItemList fp = (right sort) . P.parse readItems fp <$> readFile fp
   where
      sort = sortBy (comparing _itemExpiration)

-- |Splits an item list by expiration date. The first part contains the
--  not-yet-expired items, the second part contains the expired ones.
partitionByDate :: Date -> [Item] -> ([Item], [Item])
partitionByDate today = partition f
   where
      f x = _itemExpiration x >= today

showItem :: Item -> String
showItem (Item name contype qty expiration) = mconcat
   [show qty, " ", name, " (", show contype, "): ", showDate expiration]

showDate :: Date -> String
showDate (Date y m d) = mconcat [show d, ".", show m, ".", show y]

main :: IO ()
main = do
   today <- mkCurrentDate
   items <- readItemList "food.csv"
   case items of
      Left err -> print err
      Right items' -> do
         let (good, expired) = partitionByDate today items'
         putStrLn "Expired: "
         putStrLn "------------------------------"
         mapM_ (putStrLn . showItem) expired
         putStrLn "Good: "
         putStrLn "------------------------------"
         mapM_ (putStrLn . showItem) good
