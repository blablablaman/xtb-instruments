{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (when, zipWithM)
import Data.Aeson (Object, Value (Object), decode, (.:))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Foldable (traverse_)
import Data.List (genericLength)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text as T (Text, dropWhileEnd, init, lines, pack)
import Data.Text.IO as TIO (hPutStrLn, putStr, putStrLn, readFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    Option,
    Scheme (Https),
    Url,
    defaultHttpConfig,
    header,
    https,
    lbsResponse,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
  )
import Params
  ( Params (..),
    cmdLineParser,
  )
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO
  ( IOMode (WriteMode),
    hClose,
    hFlush,
    openFile,
    stdout,
  )
import System.Random (randomRIO)
import TextShow (TextShow (showb, showt), toString)
import Types

newtype Symbol = Symbol {symbolValue :: Text} deriving (Eq, Ord, Show)

url :: Url 'Https
url = https "www.xtb.com" /: "api" /: "pl" /: "instruments" /: "get"

headers :: Option scheme
headers =
  mconcat
    [ header "Referer" "https://www.xtb.com/pl/oferta/informacje-o-rachunku/specyfikacja-instrumentow",
      header "User-Agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
      header "X-Requested-With" "XMLHttpRequest"
    ]

loadSymbols :: FilePath -> IO (Set.Set Symbol)
loadSymbols fpath = do
  symbols <- TIO.readFile fpath
  return $ Set.fromList (Symbol <$> T.lines symbols)

saveSymbols :: Set.Set Symbol -> FilePath -> IO ()
saveSymbols symbols fpath = do
  file <- openFile fpath WriteMode
  traverse_ (TIO.hPutStrLn file . symbolValue) symbols
  hClose file

updateSymbols :: Set.Set Symbol -> FilePath -> IO ()
updateSymbols symbols fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      current <- loadSymbols fpath
      let (added, removed) = diff current symbols
      if (not . null) added || (not . null) removed
        then do
          printDiff added removed
          saveSymbols symbols fpath
        else TIO.putStrLn "No symbols changed."
    else do
      TIO.putStrLn $ "File " <> T.pack fpath <> " does not exist, creating..."
      saveSymbols symbols fpath

-- | old new -> (added, removed)
diff :: Set.Set Symbol -> Set.Set Symbol -> (Set.Set Symbol, Set.Set Symbol)
diff old new = (added, removed)
  where
    added = Set.difference new old
    removed = Set.difference old new

printDiff :: Set.Set Symbol -> Set.Set Symbol -> IO ()
printDiff added removed = do
  if null added || null removed
    then TIO.putStrLn "No symbols changed."
    else do
      TIO.putStrLn "Symbols added:"
      traverse_ (TIO.putStrLn . symbolValue) added
      TIO.putStrLn "Symbols removed:"
      traverse_ (TIO.putStrLn . symbolValue) removed

-- Przy pierwszym żądaniu, w parametrze _ idzie coś co wygląda na czas
-- załadowania strony w ms, który przy każdym kolejnym jest zwiększany o 1 ms.
-- Tutaj ten czas po prostu zmyślam, a obyłoby się i bez tego, bo jeżeli się
-- tego nie wyśle, to API i tak działa tak samo.
underscore :: IO Int
underscore = liftA2 (-) millis rndval
  where
    millis = round . (1000 *) <$> getPOSIXTime
    rndval = randomRIO (2500, 10000)

data InstrumentsResponse = InstrumentsResponse
  { totalSymbols :: Maybe Int,
    symbols :: Maybe [Symbol]
  }
  deriving (Show)

parseInstrumentsResponse :: Text -> Maybe Text -> Object -> Parser InstrumentsResponse
parseInstrumentsResponse instrumentType country v = do
  instrumentCounts <- v .: "instrumentCounts"
  instrumentsCollectionLimited <- v .: "instrumentsCollectionLimited"
  case instrumentsCollectionLimited of
    (Object collection) -> do
      instrs <- collection .: Key.fromText instrumentType
      case country of
        Just c -> do
          count <- instrumentCounts .: Key.fromText (instrumentType <> "-" <> c)
          countryObj <- instrs .: Key.fromText c
          instruments :: KM.KeyMap Object <- countryObj .: "instruments"
          let symbols = Just $ Symbol . stripCountry . Key.toText <$> KM.keys instruments
          return $ InstrumentsResponse count symbols
          where
            stripCountry = T.init . T.dropWhileEnd (/= '.')
        Nothing -> do
          count <- instrumentCounts .: Key.fromText instrumentType
          let symbols = Just $ Symbol . Key.toText <$> KM.keys instrs
          return $ InstrumentsResponse count symbols
    _ -> return $ InstrumentsResponse Nothing Nothing

getQueryParams :: Text -> Maybe Text -> Int -> Int -> Option scheme
getQueryParams instrumentType country page mysteryVal =
  mconcat
    [ "instrumentTypeSlug" =: instrumentType,
      "page" =: page,
      "_" =: mysteryVal
    ]
    <> maybe mempty ("country" =:) country

downloadPage :: Text -> Maybe Text -> Int -> Int -> IO InstrumentsResponse
downloadPage instrumentType country page mysteryVal = runReq defaultHttpConfig $ do
  r <- req GET url NoReqBody lbsResponse (headers <> getQueryParams instrumentType country page mysteryVal)
  let response = decode (responseBody r)
  case response of
    (Just resp) -> do
      let parsed = parseMaybe (parseInstrumentsResponse instrumentType country) resp
      case parsed of
        (Just instrumentsResponse) -> return instrumentsResponse
        _ -> error "Parsing response failed."
    _ -> error "Decoding response JSON failed."

downloadPageAndPrint :: Text -> Maybe Text -> Int -> Int -> Int -> IO InstrumentsResponse
downloadPageAndPrint instrumentType country total page mysteryVal = do
  TIO.putStr $ "Downloading page " <> showt page <> "/" <> showt total <> "...\r"
  hFlush stdout
  downloadPage instrumentType country page mysteryVal

fetchSymbols :: InstrumentType -> Maybe Country -> IO (Maybe (Set.Set Symbol))
fetchSymbols instrumentType country = do
  mysteryValue <- underscore
  let it = showt instrumentType
  let c = showt <$> country
  TIO.putStr "Downloading page 1...\r"
  response <- downloadPage it c 1 mysteryValue
  case response of
    InstrumentsResponse (Just total) (Just syms) -> do
      case c of
        Just countryName -> TIO.putStrLn $ showt total <> " " <> countryName <> " " <> it <> " symbols found."
        Nothing -> TIO.putStrLn $ showt total <> " " <> it <> " symbols found."
      let lst = ceiling ((realToFrac total :: Double) / genericLength syms)
      responses <- zipWithM (downloadPageAndPrint it c lst) [2 .. lst] [mysteryValue + 1 ..]
      return $ Set.fromList <$> Just syms <> (concat <$> traverse symbols responses)
    InstrumentsResponse _ _ -> error "First page download failed."

genFilename :: InstrumentType -> Maybe Country -> FilePath
genFilename it c = toString $ "xtb_" <> showb it <> maybe "" (("_" <>) . showb) c <> ".txt"

work :: Params -> IO ()
work params = do
  let it = instrumentType params
  let c = country params

  if it `elem` [Cashstocks, Shares]
    then when (isNothing c) $ do
      TIO.putStrLn $ showt it <> " instrument type requires a country."
      exitFailure
    else when (isJust c) $ do
      TIO.putStrLn $ showt it <> " instrument type doesn't require a country."
      exitFailure

  symbols <- fetchSymbols it c
  TIO.putStrLn ""
  case symbols of
    Just syms -> do
      let filename = fromMaybe (genFilename it c) (outputFile params)
      updateSymbols syms filename
    Nothing -> TIO.putStrLn "Download failed." >> exitFailure

main :: IO ()
main = cmdLineParser >>= work