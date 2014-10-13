{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , RecordWildCards
  , DeriveGeneric
  , OverloadedStrings
  #-}

module Vaultaire.Collectors.Twitter.Types where

import qualified Data.ByteString.Char8 as BSC
import           Data.Word
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Monoid
import           Data.Text(Text)
import qualified Data.Text as T
import           GHC.Generics
import           System.IO
import           System.Log.FastLogger
import           Web.Authenticate.OAuth

import           Marquise.Client
import           Vaultaire.Types

data CollectorOptions = CollectorOptions
    { optNamespace            :: String
    , optLogLevel             :: LogLevel
    }

data CollectorState = CollectorState
    { collectorSpoolFiles :: SpoolFiles
    , collectorAuth       :: OAuth
    , collectorCred       :: Credential
    }

data Tweet = Tweet
    { text       :: Text
    , created_at :: String
    , tweet_id   :: Word64
    } deriving (Show, Generic)

instance FromJSON Tweet where
    parseJSON (Object v) =  Tweet
                        <$> v .: "text"
                        <*> v .: "created_at"
                        <*> v .: "id"
instance ToJSON Tweet

data Pepito = PepitoIsOut
            | PepitoIsBack
  deriving Show

newtype CollectorMonad a = CollectorMonad {
    unCollector :: ReaderT CollectorOptions (StateT CollectorState IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState CollectorState, MonadReader CollectorOptions, MonadThrow)

instance MonadLogger CollectorMonad where
    monadLoggerLog _ _ level msg = do
        CollectorState{..}   <- get        
        CollectorOptions{..} <- ask
        when (level >= optLogLevel) $ liftIO $ do
            currTime <- getCurrentTimeNanoseconds
            let logPrefix = mconcat $ map toLogStr [showLevel level, " ",  show currTime, " "]
            let output = fromLogStr $ logPrefix <> toLogStr msg
            BSC.hPutStrLn stdout output
            when (level == LevelError) $ BSC.hPutStrLn stderr output
      where
        showLevel LevelDebug     = "[Debug]"
        showLevel LevelInfo      = "[Info]"
        showLevel LevelWarn      = "[Warning]"
        showLevel LevelError     = "[Error]"
        showLevel (LevelOther l) = concat ["[", show l, "]"]

logDebugStr   :: MonadLogger m => String -> m ()
logDebugStr   = logDebugN   . T.pack

logInfoStr    :: MonadLogger m => String -> m ()
logInfoStr    = logInfoN    . T.pack

logWarnStr    :: MonadLogger m => String -> m ()
logWarnStr    = logWarnN    . T.pack

logErrorStr   :: MonadLogger m => String -> m ()
logErrorStr   = logErrorN   . T.pack

logOtherStr   :: MonadLogger m => LogLevel -> String -> m ()
logOtherStr l = logOtherN l . T.pack
