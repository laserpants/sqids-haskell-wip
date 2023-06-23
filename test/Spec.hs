{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Sqids.Sqids (shuffle)
import Control.Monad (forM_)
import Data.List.Split (splitOn)
import Data.Text (pack)

testShuffle :: SpecWith ()
testShuffle = do
  file <- runIO $ readFile "test/data/shuffle.txt"
  describe "shuffle" $
    forM_ (lines file) $ \line -> do
      case splitOn "|" line of
        input : result : _ ->
          it input (shuffle (pack input) == pack result)
        _ ->
          error "testShuffle: bad input"

main :: IO ()
main = do
  hspec $ do
    testShuffle
