module Converter.Types where

import ClassyPrelude

-- |Metric and Imperial units of weight.
data WeightUnit =
   MilliGram
   | Gram
   | DekaGram
   | KiloGram
   | MetricTon
   | Ounce
   | Pound
   | Stone
   deriving (Eq, Ord, Show, Enum, Bounded)

-- |Metric and Imperial units of volume.
data VolumeUnit =
   MilliLiter
   | CentiLiter
   | DeciLiter
   | Liter
   | HectoLiter
   | FluidOunce
   | Cup
   | Pint
   | Quart
   | Gallon
   deriving (Eq, Ord, Show, Enum, Bounded)

-- |Metric and Imperial units of length.
data LengthUnit =
   MilliMeter
   | CentiMeter
   | Meter
   | KiloMeter
   | Inch
   | Foot
   | Yard
   | Mile
   deriving (Eq, Ord, Show, Enum, Bounded)

-- |Metric and Imperial units of area.
data AreaUnit =
   SquareMilliMeter
   | SquareCentiMeter
   | SquareMeter
   | SquareKiloMeter
   | Hectare
   | Acre
   | SquareInch
   | SquareFoot
   | SquareYard
   deriving (Eq, Ord, Show, Enum, Bounded)

-- |Sum of all possible units
data Unit =
   WU WeightUnit
   | VU VolumeUnit
   | LU LengthUnit
   | AU AreaUnit
   deriving (Eq, Ord, Show)

-- |Represents a (possibly incomplete) conversion from
--  one unit to another.
data Conversion = Conversion
   { _conversionSource :: (Maybe Unit, Maybe Double),
     _conversionTarget :: (Maybe Unit, Maybe Double)
   }
   deriving (Eq, Ord, Show)

-- |The possible stages of the user's input.
--  The stages follow each other, except for the 'ErrorStage',
--  which can be reached from anywhere.
data InputStage =
   InitStage
   | SourceNumEntered
   | SourceUnitEntered
   | ToEntered
   | TargetUnitEntered
   | TargetNumEntered
   | ErrorStage
   deriving (Eq, Ord, Enum, Bounded)


-- |Signifies a value that has been successfully parsed.
newtype Good a = Good a

-- |Signifies a value that can't be parsed.
newtype Bad a = Bad a
