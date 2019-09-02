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
parsePhone = PhoneNumber <$> parseNumberingPlanArea <*> parseExchange <*> parseLineNumber

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = do
  numberingPlanArea <- parsePhonePart 3
  skipMany (char '-')
  return numberingPlanArea

parseExchange :: Parser Exchange
parseExchange = do
  numberingPlanArea <- parsePhonePart 3
  skipMany (char '-')
  return numberingPlanArea

parseLineNumber :: Parser LineNumber
parseLineNumber = parsePhonePart 4 <* eof

parsePhonePart :: Int -> Parser Int
parsePhonePart digits = read <$> count digits digit

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

