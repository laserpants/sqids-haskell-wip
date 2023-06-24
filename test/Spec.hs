{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad (mapM_)
import Data.List.Split (splitOn)
import Data.Text (pack)
import Sqids.Sqids (shuffle)
import Sqids.Utils (swapChars)
import Test.Hspec

withTestData :: FilePath -> ([String] -> SpecWith ()) -> SpecWith ()
withTestData name mu = do
  file <- runIO $ readFile ("test/data/" <> name <> ".txt")
  describe name $ mapM_ (mu . splitOn "|") (lines file)

testSwapChars :: SpecWith ()
testSwapChars = do
  withTestData "swapChars" $ \case
    m : n : input : result : _ ->
      let msg = input <> " " <> m <> " " <> n
       in it msg (swapChars (read m) (read n) (pack input) == pack result)
    _ ->
      error "testSwapChars: bad input"

testShuffle :: SpecWith ()
testShuffle = do
  withTestData "shuffle" $ \case
    input : result : _ ->
      it input (shuffle (pack input) == pack result)
    _ ->
      error "testShuffle: bad input"

main :: IO ()
main =
  hspec $ do
    testShuffle
    testSwapChars
