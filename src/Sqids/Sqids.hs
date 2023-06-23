{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sqids.Sqids () where

import Control.Monad.State.Strict
import Data.Char (ord)
import Data.Text (Text)

import qualified Data.Text as Text

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data SqidsState = SqidsState
  { alphabet  :: String    
  -- ^ URL-safe characters
  , minLength :: Int       
  -- ^ The minimum allowed length of IDs
  , blacklist :: [String]  
  -- ^ A list of words that must never appear in IDs
  }

defaultSqidsState :: SqidsState
defaultSqidsState = SqidsState
  { alphabet  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = undefined
  }

newtype Sqids a = Sqids { exposeSqids :: State SqidsState a }
  deriving (Functor, Applicative, Monad)

encode :: (Integral n) => [n] -> Sqids String
encode numbers = do
  undefined
--  abc <- getAlphabet
--  pure "hello"
--  getAlphabet >=> \abc -> pure "hello"

decode :: (Integral n) => String -> Sqids [n]
decode = undefined

encodeNumbers :: (Integral n) => Bool -> [n] -> Sqids String
encodeNumbers partitioned numbers =
  undefined

shuffle :: Text -> Text
shuffle alphabet =
  foldr mu alphabet (reverse ixs)
  where 
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    len = Text.length alphabet
    --
    mu :: (Int, Int) -> Text -> Text
    mu (i, j) chars =
      let r = (i * j + ordAt i + ordAt j) `mod` len
       in swapChars i r chars
      where
        ordAt = ord . Text.index chars

swapChars :: Int -> Int -> Text -> Text
swapChars m n input =
  replaceCharAtIndex n charAtIndexM (replaceCharAtIndex m charAtIndexN input)
  where
    charAtIndexM, charAtIndexN :: Char
    charAtIndexM = Text.index input m
    charAtIndexN = Text.index input n

replaceCharAtIndex :: Int -> Char -> Text -> Text
replaceCharAtIndex n char input =
  lhs <> Text.cons char rhs
  where
    lhs = Text.take n input
    rhs = Text.drop (n + 1) input

toId :: (Integral n) => n -> String -> String
toId number alphabet =
  undefined

toNumber :: (Integral n) => String -> String -> n
toNumber _id alphabet =
  undefined

isBlockedId :: String -> Bool
isBlockedId _id =
  undefined

