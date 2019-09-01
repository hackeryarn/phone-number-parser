module Main (main) where

import Test.Hspec
import Text.Trifecta
import PhoneNumberParser

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  let ps = parseString
      pspn = ps parsePhone mempty
  describe "parsePhone" $ do
    it "parses dash separate phone number" $ do
      let r = pspn "123-456-7890"
      maybeSuccess r `shouldBe` Just (PhoneNumber 123 456 7890)
