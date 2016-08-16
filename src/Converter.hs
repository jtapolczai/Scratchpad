module Converter where

import ClassyPrelude

import qualified Data.Bimap as BM
import qualified Data.Map as M
import Control.Lens hiding (cons, snoc)
import qualified Text.Parsec as P
import Data.Text (inits)
import Text.Read (read)

import Converter.Data
import Converter.TH
import Converter.Types

makePrisms ''WeightUnit
makePrisms ''VolumeUnit
makePrisms ''LengthUnit
makePrisms ''AreaUnit
makePrisms ''Unit
makeFields ''Conversion

-- |Gets the short name of a unit.
unitNames :: BM.Bimap Unit Text
unitNames = BM.fromList $
   (map (over _1 WU) $ BM.toList wNames) ++
   (map (over _1 VU) $ BM.toList vNames) ++
   (map (over _1 LU) $ BM.toList lNames) ++
   (map (over _1 AU) $ BM.toList aNames)

-- |Gets the Unit associated with a name.
--  __Unsafe__ in case of non-existent units.
bml :: Text -> Unit
bml = (BM.!>) unitNames

-- |Gets the short name of a unit.
unitScale :: M.Map Unit Double
unitScale = M.fromList $
   (map (over _1 WU) $ M.toList wScales) ++
   (map (over _1 VU) $ M.toList vScales) ++
   (map (over _1 LU) $ M.toList lScales) ++
   (map (over _1 AU) $ M.toList aScales)

-- |Gets the name of the quantity the unit measures.
unitKind :: Getter Unit Text
unitKind = to go
   where
      go WU{} = "weight"
      go VU{} = "volume"
      go LU{} = "length"
      go AU{} = "area"

-- |All units __and their prefixes__, starting with the longest.
unitPrefixes :: [Text]
unitPrefixes =
   sortBy (flip $ comparing length)
   $ concatMap inits
   $ BM.keysR unitNames

-- |Gets the stage of a 'Conversion'.
stage :: Getter Conversion InputStage
stage = to go
   where
      go (Conversion (Nothing, Nothing) (Nothing, Nothing)) = InitStage
      go (Conversion (Just _, Nothing) (Nothing, Nothing)) = SourceUnitEntered
      go (Conversion (Just _, Just _ ) (Nothing, Nothing)) = SourceNumEntered
      go (Conversion (Just _, Just _ ) (Just _, Nothing)) = TargetUnitEntered
      go (Conversion (Just _, Just _ ) (Just _, Just _)) = TargetNumEntered
      go _ = ErrorStage


-- |Tries to perform a conversion. This will succeed if
--
-- * the left unit and amount are given,
-- * the right unit is given, and
-- * the left and right units are of the same kind.
--
-- In case of failure, an error text is returned.
convert :: Conversion -> Either Text Conversion
convert (Conversion (Nothing,_) _) = Left "Enter a source unit!"
convert (Conversion (_,Nothing) _) = Left "Enter an amount!"
convert (Conversion _ (Nothing,_)) = Left "Enter a target unit!"
convert c@(Conversion (Just su, Just sa) (Just tu,_)) =
   if $(isSame ''Unit) su tu
   then Right $ c & target . _2 .~ Just ta
   else Left ("The units must measure the same quantity!\n(" ++
               (unitNames BM.! su) ++ " measures " ++ (su ^. unitKind) ++
               (unitNames BM.! tu) ++ " " ++ (tu ^. unitKind) ++ ")")
   where
      ta = sa * (unitScale M.! tu / unitScale M.! su)


-- |Processes a character the user entered.
procChar
   -- |Input character @c@.
   :: Char
   -- |The entered text @line@ so far.
   -> Text
   -- |Current stage of the input.
   -> InputStage
   -- |The resultant @line@, and possibly an error.
   --  The Int indicates the position (0-based, inclusive) at which
   --  parsing failed, if it failed.
   --  The second text is an error message.
   --  It does __not__ hold that @line ++ [c] = line'@.
   -> (Text, Maybe (Int, Text))
procChar c line stage = case partialParse (line `snoc` c) of
   (Good good, Left (Bad err)) -> (line `snoc` c, Just (length good, errMsg))
   (Good good, Right conv) -> (line `snoc` c, Nothing)
   where
      errMsg = "The format is <amount> <source unit> to <target unit>"


-- |Parser a line of input @L@ as far as possible. @L@ is
--  partitioned into @(G,B)@. @G@ is the initial part
--  s.t. @G++A@ for some string @A@ will represent a valid conversion,
--  and @B@ is the non-parseable, final part of the string.
--  For wholly parseable strings, @G == L@ and @null B@.
--
--  If @G@ can be turned into a (partial) conversion, we return that too.
partialParse :: Text -> (Good Text, Either (Bad Text) Conversion)
partialParse t = if null t then (Good "", Right emptyConv)
                           else parseRes
   where
      emptyConv = Conversion (Nothing, Nothing) (Nothing, Nothing)

      reportErr e = (Good $ take pos t, Left $ Bad $ drop pos t)
         where
            pos = P.sourceColumn $ P.errorPos e

      parseRes = either (reportErr)
                        (\r -> (Good t, Right r))
                        (P.runParser p () "Input" t)

      p :: P.Parsec Text () Conversion
      p = do
         P.many P.space
         sa <- parseDouble
         P.many P.space
         (wholeUnit, su) <- parseUnit
         if wholeUnit -- If the whole source unit was entered, we continue parsing
         then do
            P.many1 P.space
            P.string "to"
            P.many1 P.space
            trg <- (Left <$> pFinished) <|> (Right <$> parseUnit)
            P.spaces
            -- This isn't prettty, but we have to handle partially entered input.
            return $ case trg of
               Left (ta, (True, tu)) ->
                  Conversion (Just $ bml su,Just sa) (Just $ bml tu, Just ta)
               Left (ta, (False, _)) ->
                  Conversion (Just $ bml su,Just sa) (Nothing, Just ta)
               Right (True, tu) ->
                  Conversion (Just $ bml su,Just sa) (Just $ bml tu, Nothing)
               Right (False, _) ->
                  Conversion (Just $ bml su,Just sa) (Nothing, Nothing)
         else return $ Conversion (Nothing, Just sa) (Nothing, Nothing)

      pFinished :: P.Parsec Text () (Double, (Bool, Text))
      pFinished = do
         trgNum <- parseDouble
         P.spaces
         trgUnit <- parseUnit
         return (trgNum, trgUnit)

      -- |Parses a string that represents a unit or the initial part of a unit.
      --  The return bool will be True iff the string represents a unit's entire
      --  name.
      parseUnit :: P.Parsec Text () (Bool, Text)
      parseUnit = do
         unit <- P.choice (map wordP unitPrefixes)
         return (unit `BM.memberR` unitNames , unit)

         where
            wordP :: Text -> P.Parsec Text () Text
            wordP x = P.try $ fmap pack (P.string $ unpack x)

      -- |Parses the string @[+-]?[0-9]*[.]?[0-9]*@ as a 'Double'.
      parseDouble :: P.Parsec Text () Double
      parseDouble = signed <|> unsigned

      -- |Parses a 'Double' with a sign.
      signed :: P.Parsec Text () Double
      signed = do
         sign <- P.char '+' <|> P.char '-'
         num <- unsigned
         return $ (if sign == '-' then negate else id) num

      -- |Parses a 'Double' without a sign.
      unsigned :: P.Parsec Text () Double
      unsigned = do
         d1 <- P.many P.digit
         dot <- maybe "" (:[]) <$> P.optionMaybe (P.char '.')
         d2 <- P.many P.digit
         let d1' = if null d1 then "0" else d1
             d2' = if null d2 then "0" else d2
             num = if null dot then d1' else d1' <> dot <> d2'
         return $ read num
