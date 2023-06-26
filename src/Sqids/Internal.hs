{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Sqids.Internal
  ( shuffle
  , sqidsVersion
  , sqidsOptions
  , defaultSqidsOptions
  , isBlockedId
  , runSqids
  , runSqidsT
  , sqids
  , sqidsT
  , SqidsOptions(..)
  , Sqids(..)
  , SqidsT(..)
  , MonadSqids(..)
  , SqidsError(..)
  , Verified(..)
  , toId
  , toNumber
  , encodeNumbers
  ) where

import Control.Monad.Except (ExceptT, runExceptT, MonadError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT, MonadState, MonadTrans, evalStateT, gets, modify, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord, isDigit)
import Data.List (foldl')
import Data.Text (Text)
import Sqids.Utils.Internal (swapChars, wordsNoLongerThan, findChar, modifyM)

import qualified Data.Text as Text

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data SqidsOptions = SqidsOptions
  { alphabet  :: Text
  -- ^ URL-safe characters
  , minLength :: Int
  -- ^ The minimum allowed length of IDs
  , blacklist :: [Text]
  -- ^ A list of words that must never appear in IDs
  } deriving (Show, Eq, Ord)

newtype Verified a = Verified { getVerified :: a }
  deriving (Show, Read, Eq, Ord)

data SqidsError
  = SqidsAlphabetTooShortError
  deriving (Show, Read, Eq, Ord)

emptySqidsOptions :: SqidsOptions
emptySqidsOptions = SqidsOptions mempty 0 []

defaultSqidsOptions :: SqidsOptions
defaultSqidsOptions = SqidsOptions
  { alphabet  = Text.pack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = []
  }

type SqidsStack m = StateT (Verified SqidsOptions) (ExceptT SqidsError m)

newtype SqidsT m a = SqidsT { unwrapSqidsT :: SqidsStack m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Verified SqidsOptions)
    , MonadError SqidsError
    )

instance MonadTrans SqidsT where
  lift = SqidsT . lift . lift

newtype Sqids a = Sqids { unwrapSqids :: SqidsT Identity a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Verified SqidsOptions)
    , MonadError SqidsError
    , MonadSqids
    )

runSqidsT :: (Monad m) => SqidsOptions -> SqidsT m a -> m (Either SqidsError a)
runSqidsT options _sqids =
  runExceptT (evalStateT (unwrapSqidsT withOptions) (Verified emptySqidsOptions))
  where
    withOptions = sqidsOptions options >>= put >> _sqids

sqidsT :: (Monad m) => SqidsT m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options . unwrapSqids

sqids :: Sqids a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

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
  --
  getAlphabet = gets (alphabet . getVerified)
  setAlphabet newAlphabet =
    modifyM $ \(Verified o) ->
      sqidsOptions o{ alphabet = newAlphabet }
  --
  getMinLength = gets (minLength . getVerified)
  setMinLength newMinLength =
    modifyM $ \(Verified o) ->
      sqidsOptions o{ minLength = newMinLength }
  --
  getBlacklist = gets (blacklist . getVerified)
  setBlacklist newBlacklist =
    modifyM $ \(Verified o) ->
      sqidsOptions o{ blacklist = newBlacklist }

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

-- | SqidsOptions constructor
sqidsOptions :: (MonadSqids m) => SqidsOptions -> m (Verified SqidsOptions)
sqidsOptions SqidsOptions{..} =
  pure $ Verified $ SqidsOptions
    { alphabet  = alphabet
    , minLength = minLength
    , blacklist = blacklist
    }
--  where -- TODO

-- | Internal function that encodes an array of unsigned integers into an ID
encodeNumbers :: (MonadSqids m, Integral n) => [n] -> Bool -> m Text
encodeNumbers numbers partitioned = do
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
