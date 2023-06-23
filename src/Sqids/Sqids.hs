{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sqids.Sqids () where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT, MonadState, MonadTrans, evalStateT, gets, modify, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord)
import Data.List (foldl')
import Data.Text (Text)

import qualified Data.Text as Text

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data SqidsState = SqidsState
  { alphabet  :: Text
  -- ^ URL-safe characters
  , minLength :: Int       
  -- ^ The minimum allowed length of IDs
  , blacklist :: [Text]  
  -- ^ A list of words that must never appear in IDs
  }

defaultSqidsState :: SqidsState
defaultSqidsState = SqidsState
  { alphabet  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = []
  }

newtype SqidsT m a = SqidsT { unwrapSqidsT :: StateT SqidsState m a }
  deriving (Functor, Applicative, Monad, MonadState SqidsState, MonadTrans)

newtype Sqids a = Sqids { unwrapSqids :: SqidsT Identity a }
  deriving (Functor, Applicative, Monad, MonadState SqidsState, MonadSqids)

runSqidsT :: (Monad m) => SqidsState -> SqidsT m a -> m a
runSqidsT _state = flip evalStateT _state . unwrapSqidsT

sqidsT :: (Monad m) => SqidsT m a -> m a
sqidsT = runSqidsT defaultSqidsState

runSqids :: SqidsState -> Sqids a -> a
runSqids _state = runIdentity . runSqidsT _state . unwrapSqids

sqids :: Sqids a -> a
sqids = runSqids defaultSqidsState

class MonadSqids m where
  encode :: (Integral n) => [n] -> m String
  decode :: (Integral n) => String -> m [n]
  getAlphabet :: m Text
  setAlphabet :: Text -> m ()
  getMinLength :: m Int
  setMinLength :: Int -> m ()
  getBlacklist :: m [Text]
  setBlacklist :: [Text] -> m ()

instance (Monad m) => MonadSqids (SqidsT m) where
  encode = undefined
  decode = undefined
  getAlphabet = gets alphabet
  setAlphabet alphabet = modify $ \old -> old{ alphabet = alphabet }
  getMinLength = gets minLength
  setMinLength minLength = modify $ \old -> old{ minLength = minLength }
  getBlacklist = gets blacklist
  setBlacklist blacklist = modify $ \old -> old{ blacklist = blacklist }

instance (Monad m, MonadSqids m) => MonadSqids (StateT s m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m) => MonadSqids (ExceptT e m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m) => MonadSqids (ReaderT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m) => MonadSqids (MaybeT m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m) => MonadSqids (ContT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (Monad m, MonadSqids m) => MonadSqids (SelectT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

encodeNumbers :: (Integral n) => Bool -> [n] -> Sqids String
encodeNumbers partitioned numbers =
  undefined

shuffle :: Text -> Text
shuffle alphabet = foldl' mu alphabet ixs
  where 
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    len = Text.length alphabet
    --
    mu :: Text -> (Int, Int) -> Text
    mu chars (i, j) =
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
replaceCharAtIndex n char input = lhs <> Text.cons char rhs
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

--

example :: Text
example = sqids $ do
  setAlphabet "xyz"
  getAlphabet

example2 :: IO ()
example2 = do 
  s <- sqidsT $ do
    setAlphabet "xyz"
    getAlphabet
  print s
  pure ()