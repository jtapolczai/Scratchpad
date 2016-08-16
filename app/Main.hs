{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import ClassyPrelude
import Data.Char (chr)
import Foreign.C.Types
import System.Console.ANSI
-- import System.IO

import Converter

main :: IO ()
main = do
   hSetBuffering stdin NoBuffering
   main' ""
   where
      main' :: Text -> IO ()
      main' inp = do
         c <- getChar'
         (outp, err) <- procChar c inp
         main' outp



-- |A version of 'getChar' that actually works with 'NoBuffering'.
--  Due to a <https://ghc.haskell.org/trac/ghc/ticket/2189 bug> in GHC,
--  'getChar' requires pressing /Enter/.
getChar' = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

consoleUI :: IO ()
consoleUI = go "" InitStage
   where
      setWhite = setSGR [SetColor Foreground Vivid White]
      setRed = setSGR [SetColor Foreground Dull Red]
      setRedV = setSGR [SetColor Foreground Vivid Red]
      setDef = setSGR [SetColor Foreground Dull White]

      go inp stage = do
         c <- getChar'

         if c == '\ESC' then return ()
         else do
            when (c == '\n') cursorUpLine 1
            (outp, err) <- procChar c inp stage
            clearScreen
            setCursorColumn 0
            case err of
               Nothing -> do
                  setWhite
                  putStr outp
               Just (errPos, errMsg) -> do
                  setWhite
                  putStr (take errPos outp)
                  setRed
                  putStr (drop errPos outp)
            go
