{-# LANGUAGE OverloadedStrings #-}

module Params where

import Options.Applicative
  ( Parser,
    argument,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    optional,
    progDesc,
    short,
    strOption,
    (<**>),
  )
import Types

data Params = Params
  { instrumentType :: InstrumentType,
    country :: Country,
    outputFile :: Maybe FilePath
  }

mkParams :: Parser Params
mkParams =
  Params
    <$> argument
      (maybeReader (`lookup` instrumentTypes))
      ( metavar "INSTRUMENT_TYPE"
          <> help "Instrument type. One of: forex, indices, cashstocks, commodities, shares, cryptocurrencies, etfcfd, etfs."
      )
    <*> argument
      (maybeReader (`lookup` countries))
      ( metavar "COUNTRY"
          <> help "Country. One of: us, pl, uk, ukint, cz, fr, de, it, pt, es, be, nl, ch, dk, se, fi, no."
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILENAME"
              <> help "Output filename. Default: xtb_<instrument_type>_<country>.txt"
          )
      )
  where
    instrumentTypes =
      [ ("forex", Forex),
        ("indices", Indices),
        ("cashstocks", Cashstocks),
        ("commodities", Commodities),
        ("shares", Shares),
        ("cryptocurrencies", Cryptocurrencies),
        ("etfcfd", EtfCfd),
        ("etfs", Etfs)
      ]
    countries =
      [ ("us", Us),
        ("pl", Pl),
        ("uk", Uk),
        ("ukint", UkInt),
        ("cz", Cz),
        ("fr", Fr),
        ("de", De),
        ("it", It),
        ("pt", Pt),
        ("es", Es),
        ("be", Be),
        ("nl", Nl),
        ("ch", Ch),
        ("dk", Dk),
        ("se", Se),
        ("fi", Fi),
        ("no", No)
      ]

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc "XTB instrument list downloader")