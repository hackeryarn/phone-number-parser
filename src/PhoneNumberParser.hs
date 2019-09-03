module PhoneNumberParser
       ( someFunc
       , parsePhone
       , PhoneNumber (..)
       ) where

import Control.Applicative
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
parsePhone = PhoneNumber <$> parseNumberingPlanArea <*> parseThreeWithSeparator <*> parseLineNumber

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = skipLeadingOne *> (parseThreeWithSeparator <|> parseParenthesesNumberingPlanArea)

skipLeadingOne :: Parser ()
skipLeadingOne = skipOptional (string "1-") 

parseParenthesesNumberingPlanArea :: Parser NumberingPlanArea
parseParenthesesNumberingPlanArea = char '(' *> parsePhonePart 3 <* char ')' <* space

parseThreeWithSeparator :: Parser Int
parseThreeWithSeparator = do
  numberingPlanArea <- parsePhonePart 3
  skipOptional (char '-')
  return numberingPlanArea

parseLineNumber :: Parser LineNumber
parseLineNumber = parsePhonePart 4 <* eof

parsePhonePart :: Int -> Parser Int
parsePhonePart digits = read <$> count digits digit

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

