{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  #-}

module Vaultaire.Collectors.Twitter.Process where

import           Control.Applicative
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Aeson
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HashMap(fromList)
import           Data.Monoid
import           Data.Text(Text)
import           Data.Text.Encoding
import           Data.Time.Format
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Locale
import           Web.Authenticate.OAuth

import           Marquise.Client
import           Vaultaire.Types

import           Vaultaire.Collectors.Twitter.Types

getSourceDict :: [(Text, Text)] -> Either String SourceDict
getSourceDict =
    makeSourceDict . HashMap.fromList

processPepito :: CollectorMonad ()
processPepito = do
    CollectorState{..} <- get
    req <- parseUrl "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=PepitoTheCat&count=50"
    signedReq <- signOAuth collectorAuth collectorCred req
    res <- liftIO $ withManager tlsManagerSettings $ \m -> httpLbs signedReq m
    let tweets = decode $ responseBody res
    case tweets of
        Nothing        -> return ()
        Just ts -> forM_ ts $ \Tweet{..} -> do
            logInfoStr $ "id = " ++ show tweet_id
            let pepitoStatus = case maybeResult $ parse pepitoParser text of
                                   Nothing -> Nothing
                                   Just "back home" -> Just PepitoIsBack
                                   Just "out"       -> Just PepitoIsOut
                                   Just _           -> Nothing
            let time = parseTime defaultTimeLocale "%a %b %d %X %z %Y" created_at
            case (pepitoStatus, time) of
                (Just status, Just t) -> do 
                    payload <- case status of
                        PepitoIsBack -> return 1
                        PepitoIsOut  -> return 0
                    let ts = convertToTimeStamp t
                    let tagList = [("source", "twitter"), ("twitter_account", "PepitoTheCat"), ("_event", "1")]
                    let addressString = mconcat $ map (\(a, b) -> encodeUtf8 (mconcat [a, ": ", b, ", "])) tagList
                    let addr = hashIdentifier addressString
                    case getSourceDict tagList of
                        Left  _ -> return ()
                        Right sd -> do
                            logInfoStr $ "Queuing simple point with payload " ++ show payload
                            liftIO $ queueSimple collectorSpoolFiles addr ts payload
                            logInfoStr $ "Queuing sd: " ++ show sd
                            liftIO $ queueSourceDictUpdate collectorSpoolFiles addr sd
                            logInfoStr $ "Timestamp: " ++ show ts
                _ -> return ()

pepitoParser :: Parser Text
pepitoParser =   string "Pépito is "
             *> (string "back home" <|> string "out")
