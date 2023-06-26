{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sqids.Internal 
  ( shuffle
  , sqidsVersion
  , defaultSqidsState
  , isBlockedId
  , runSqids
  , runSqidsT
  , sqids
  , sqidsT
  , SqidsState(..)
  , toId
  , toNumber
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT, MonadState, MonadTrans, evalStateT, gets, modify, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord, isDigit)
import Data.List (foldl')
import Data.Text (Text)
import Sqids.Utils.Internal (swapChars, wordsNoLongerThan, findChar)

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

class (Monad m) => MonadSqids m where
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
  setAlphabet new = modify $ \old -> old{ alphabet = new }
  getMinLength = gets minLength
  setMinLength new = modify $ \old -> old{ minLength = new }
  getBlacklist = gets blacklist
  setBlacklist new = modify $ \old -> old{ blacklist = new }

instance (MonadSqids m) => MonadSqids (StateT s m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

-- | Internal function that encodes an array of unsigned integers into an ID
encodeNumbers :: (Integral n) => [n] -> Bool -> Sqids Text
encodeNumbers numbers partitioned =
  undefined

shuffle :: Text -> Text
shuffle chars = foldl' mu chars ixs
  where 
    len = Text.length chars
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    --
    mu :: Text -> (Int, Int) -> Text
    mu txt (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . Text.index txt
       in swapChars i r txt

toId :: Int -> Text -> Text
toId num chars = Text.reverse (mu num)
  where
    len = Text.length chars
    mu n = 
      let next = if m == 0 then Text.empty else mu m
          (m, r) = n `divMod` len in Text.cons (Text.index chars r) next

toNumber :: Text -> Text -> Int
toNumber _id chars = Text.foldl' mu 0 _id
  where
    len = Text.length chars
    mu v c = 
      case findChar c chars of
        Just n -> len * v + n
        _ -> error "toNumber: bad input"

isBlockedId :: (MonadSqids m) => Text -> m Bool
isBlockedId _id = do
  list <- wordsNoLongerThan (Text.length theId) <$> getBlacklist
  pure (any disallowed list)
  where
    theId = Text.toLower _id
    disallowed w
      | Text.length theId <= 3 || Text.length w <= 3 = 
        -- Short words have to match exactly
        w == theId
      | Text.any isDigit w = 
        -- Look for "leetspeak" words
        w `Text.isPrefixOf` theId || w `Text.isSuffixOf` theId
      | otherwise = 
        -- Check if word appears anywhere in the string
        w `Text.isInfixOf` theId

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

example3 :: SqidsT IO ()
example3 = do
  s <- do
    setAlphabet "xyz"
    getAlphabet
  lift $ print s
  pure ()

example4 :: IO ()
example4 = sqidsT example3

example5 :: Bool
example5 = sqids $ do
  setBlacklist ["f00", "baz"]
  isBlockedId "f00"
