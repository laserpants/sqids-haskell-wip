{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sqids.Sqids () where

import Control.Monad.State

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data SqidsState = SqidsState
  { alphabet  :: String    
  -- ^ URL-safe characters
  , minLength :: Int       
  -- ^ The minimum allowed length of IDs
  , blacklist :: [String]  
  -- ^ A list of words that must not appear in IDs
  }

defaultSqidsState :: SqidsState
defaultSqidsState = SqidsState
  { alphabet  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = undefined
  }

newtype Sqids a = Sqids { exposeSqids :: State SqidsState a }
 deriving (Functor, Applicative, Monad)

--getAlphabet :: Sqids String
--getAlphabet = Sqids $ do
--  a <- gets alphabet
--  undefined

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

shuffle :: String -> String
shuffle alphabet =
  undefined

toId :: (Integral n) => n -> String -> String
toId number alphabet =
  undefined

toNumber :: (Integral n) => String -> String -> n
toNumber _id alphabet =
  undefined

isBlockedId :: String -> Bool
isBlockedId _id =
  undefined

