module PhoneNumberParser
       ( someFunc
       , parsePhone
       , PhoneNumber (..)
       ) where

import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

