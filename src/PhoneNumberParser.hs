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
parseNumberingPlanArea = parsePhonePart 3 <* char '-'

parseExchange :: Parser Exchange
parseExchange = parsePhonePart 3 <* char '-'

parseLineNumber :: Parser LineNumber
parseLineNumber = parsePhonePart 4 <* eof

parsePhonePart :: Int -> Parser Int
parsePhonePart digits = do
  numbers <- some digit
  if length numbers == digits
    then return $ read numbers
    else fail $ "Expected " ++ show digits ++ " numbers"

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

