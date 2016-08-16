module Converter.Data where

import ClassyPrelude

import qualified Data.Bimap as BM
import qualified Data.Map as M

import Converter.Types

wNames :: BM.Bimap WeightUnit Text
wNames = BM.fromList
   [(MilliGram, "mg"),
    (Gram, "g"),
    (KiloGram, "kg"),
    (MetricTon, "t"),
    (Ounce, "oz"),
    (Pound, "lbs"),
    (Stone, "st")]

vNames :: BM.Bimap VolumeUnit Text
vNames = BM.fromList
   [(MilliLiter, "ml"),
    (CentiLiter, "cl"),
    (DeciLiter, "dl"),
    (Liter, "l"),
    (HectoLiter, "hl"),
    (FluidOunce, "floz"),
    (Cup, "cup"),
    (Pint, "pt"),
    (Quart, "qt"),
    (Gallon, "g")]

lNames :: BM.Bimap LengthUnit Text
lNames = BM.fromList
   [(MilliMeter, "mm"),
    (CentiMeter, "cm"),
    (Meter, "m"),
    (KiloMeter, "km"),
    (Inch, "in"),
    (Foot, "ft"),
    (Yard, "yd"),
    (Mile, "mi")]

aNames :: BM.Bimap AreaUnit Text
aNames = BM.fromList
   [(SquareMilliMeter, "mm2"),
    (SquareCentiMeter, "cm2"),
    (SquareMeter, "m2"),
    (SquareKiloMeter, "km2"),
    (Hectare, "ha"),
    (Acre, "ac"),
    (SquareInch, "in2"),
    (SquareFoot, "ft2"),
    (SquareYard, "yd2")]

wScales ::M.Map WeightUnit Double
wScales = M.fromList
   [(MilliGram, 0.001),
    (Gram, 1),
    (DekaGram, 10),
    (KiloGram, 1000),
    (MetricTon, 1000000),
    (Ounce, 28.3495),
    (Pound, 453.592),
    (Stone, 6350.29)]

vScales :: M.Map VolumeUnit Double
vScales = M.fromList
    [(MilliLiter, 0.001),
    (CentiLiter, 0.01),
    (DeciLiter, 0.1),
    (Liter, 1),
    (HectoLiter, 100),
    (FluidOunce, 0.0295735),
    (Cup, 0.284131),
    (Pint, 0.473176),
    (Quart, 0.946353),
    (Gallon, 3.78541)]

lScales :: M.Map LengthUnit Double
lScales = M.fromList
   [(MilliMeter, 0.001),
    (CentiMeter, 0.01),
    (Meter, 1),
    (KiloMeter, 1000),
    (Inch, 0.0254),
    (Foot, 0.3048),
    (Yard, 0.9144),
    (Mile, 1609.34)]

aScales :: M.Map AreaUnit Double
aScales = M.fromList
   [(SquareMilliMeter, 0.000001),
    (SquareCentiMeter, 0.0001),
    (SquareMeter, 1),
    (SquareKiloMeter, 1000000),
    (Hectare, 10000),
    (Acre, 4046.86),
    (SquareInch, 0.00064516),
    (SquareFoot, 0.092903),
    (SquareYard, 0.836127)]
