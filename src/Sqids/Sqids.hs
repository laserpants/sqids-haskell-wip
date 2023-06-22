{-# LANGUAGE OverloadedStrings #-}
module Sqids.Sqids () where

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data Sqids a = Sqids

encode :: (Integral n) => [n] -> Sqids String
encode = undefined

decode :: (Integral n) => String -> Sqids [n]
decode = undefined
