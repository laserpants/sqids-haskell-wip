{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Data.List.Split (splitOn)
import Data.Text (pack)
import Sqids.Internal (shuffle, toId, toNumber, defaultSqidsOptions, runSqids, isBlockedId, SqidsOptions(..), Verified(..))
import Sqids.Utils.Internal (swapChars)
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

testToId :: SpecWith ()
testToId = do
  withTestData "toId" $ \case
    num : alphabet : result : _ ->
      let msg = num <> " " <> alphabet 
       in it msg (toId (read num) (pack alphabet) == pack result)
    _ ->
      error "testToId: bad input"

testToNumber :: SpecWith ()
testToNumber = do
  withTestData "toNumber" $ \case
    _id : alphabet : result : _ ->
      let msg = _id <> " " <> alphabet 
       in it msg (toNumber (pack _id) (pack alphabet) == read result)
    _ ->
      error "testToNumber: bad input"

testIsBlockedId :: SpecWith ()
testIsBlockedId = do
  withTestData "isBlockedId" $ \case
    blacklist : _id : result : _ ->
      let msg = blacklist <> " " <> _id 
          _words = pack <$> splitOn "," blacklist
          options = defaultSqidsOptions{ blacklist = _words }
       in it msg (runSqids options (isBlockedId (pack _id)) == Right (read result))
    _ ->
      error "testIsBlockedId: bad input"

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
    testToId
    testToNumber
    testIsBlockedId
