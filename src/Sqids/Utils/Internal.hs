module Sqids.Utils.Internal 
  ( swapChars
  , replaceCharAtIndex
  , wordsNoLongerThan
  , findChar
  , modifyM
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State.Strict (MonadState, get, put)

swapChars :: Int -> Int -> Text -> Text
swapChars m n input =
  replaceCharAtIndex n charAtIndexM (replaceCharAtIndex m charAtIndexN input)
  where
    charAtIndexM, charAtIndexN :: Char
    charAtIndexM = Text.index input m
    charAtIndexN = Text.index input n

replaceCharAtIndex :: Int -> Char -> Text -> Text
replaceCharAtIndex n char input = lhs <> Text.cons char rhs
  where
    lhs = Text.take n input
    rhs = Text.drop (n + 1) input

wordsNoLongerThan :: Int -> [Text] -> [Text]
wordsNoLongerThan n = filter ((<= n) . Text.length)

findChar :: Char -> Text -> Maybe Int
findChar = Text.findIndex . (==) 

modifyM :: (MonadState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put
