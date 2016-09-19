module Main where


import Test.Hspec (
  hspec, describe, it,
  shouldBe)


main :: IO ()
main = hspec (do
  describe "interaction net" (do
    it "terminates" (do
      True `shouldBe` True)))

