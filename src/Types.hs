module Types where

import TextShow (TextShow (showb))

data InstrumentType
  = Forex
  | Indices
  | Cashstocks
  | Commodities
  | Shares
  | Cryptocurrencies
  | EtfCfd
  | Etfs
  deriving (Eq, Bounded, Show)

instance TextShow InstrumentType where
  showb Forex = "forex"
  showb Indices = "indices"
  showb Cashstocks = "cashstocks"
  showb Commodities = "commodities"
  showb Shares = "shares"
  showb Cryptocurrencies = "cryptocurrencies"
  showb EtfCfd = "etfcfd"
  showb Etfs = "etfs"

data Country
  = Us
  | Pl
  | Uk
  | UkInt
  | Cz
  | Fr
  | De
  | It
  | Pt
  | Es
  | Be
  | Nl
  | Ch
  | Dk
  | Se
  | Fi
  | No
  deriving (Eq, Show)

instance TextShow Country where
  showb Us = "US"
  showb Pl = "PL"
  showb Uk = "UK"
  showb UkInt = "UK_INT"
  showb Cz = "CZ"
  showb Fr = "FR"
  showb De = "DE"
  showb It = "IT"
  showb Pt = "PT"
  showb Es = "ES"
  showb Be = "BE"
  showb Nl = "NL"
  showb Ch = "CH"
  showb Dk = "DK"
  showb Se = "SE"
  showb Fi = "FI"
  showb No = "NO"