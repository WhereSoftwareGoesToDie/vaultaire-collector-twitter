{-# LANGUAGE
    RecordWildCards
  #-}

module Vaultaire.Collectors.Twitter.State where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BSC
import           System.Environment
import           Web.Authenticate.OAuth

import           Marquise.Client

import           Vaultaire.Collectors.Twitter.Options
import           Vaultaire.Collectors.Twitter.Process
import           Vaultaire.Collectors.Twitter.Types

import           Paths_vaultaire_collector_twitter (version)

runCollector :: IO ()
runCollector = do
    opts <- parseOptions
    let logStartup = logInfoStr $ concat ["Collector version ", show version, " starting."]
    let act = logStartup >> initialiseState >> processPepito
    evalStateT (runReaderT (unCollector act) opts) (CollectorState undefined newOAuth emptyCredential)

initialiseState :: CollectorMonad ()
initialiseState = do
    CollectorOptions{..} <- ask
    [cKey, cSecret, aKey, aSecret] <- liftIO getAuth
    files <- liftIO $ createSpoolFiles optNamespace
    let auth = newOAuth { oauthServerName     = "api.twitter.com"
                        , oauthConsumerKey    = cKey
                        , oauthConsumerSecret = cSecret
                        }
    let cred = newCredential aKey aSecret
    put $ CollectorState files auth cred
  where
    getAuth = forM [ "VAULTAIRE_COLLECTOR_TWITTER_CONSUMER_KEY"
                   , "VAULTAIRE_COLLECTOR_TWITTER_CONSUMER_SECRET"
                   , "VAULTAIRE_COLLECTOR_TWITTER_ACCESS_KEY"
                   , "VAULTAIRE_COLLECTOR_TWITTER_ACCESS_SECRET"
                   ] (\s -> getEnv s >>= (return . BSC.pack))
