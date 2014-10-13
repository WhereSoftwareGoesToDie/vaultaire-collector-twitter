module Vaultaire.Collectors.Twitter.Options where

import Vaultaire.Collectors.Twitter.Types

import Options.Applicative

parseOptions :: IO CollectorOptions
parseOptions = execParser optionParser

-- | Parser which include all help info
optionParser :: ParserInfo CollectorOptions
optionParser =
    info (helper <*> collectorOptions)
    (fullDesc <>
        progDesc "Vaultaire collector for Twitter" <>
        header "vaultaire-collector-twitter - writes datapoints from Twitter feeds"
    )

-- | The parser for all options for nagios-perfdata
collectorOptions :: Parser CollectorOptions
collectorOptions = CollectorOptions
    <$> strOption
        (long "marquise-namespace"
         <> short 'n'
         <> value "twitter"
         <> metavar "MARQUISE-NAMESPACE"
         <> help "Marquise namespace to write to.")
    <*> (read <$> strOption
        (long "log-verbosity"
         <> short 'v'
         <> help "Verbosity level at which to write log output"))
