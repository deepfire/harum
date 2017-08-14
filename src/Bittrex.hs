{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Bittrex
where

import           Control.Arrow                       ((>>>))
-- import           Control.Monad.Trans.Class           (lift)
-- import           Control.Monad.Trans.Reader
import           Data.Aeson                   hiding (pairs)
import           Data.Aeson.Types             hiding (Options, Pair, pairs)
-- import qualified Data.ByteString                  as BS
import           Data.Function                       ((&))
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Proxy                          (Proxy(..))
import           Data.Text                           (Text)
import           Data.List                           (stripPrefix)
import qualified Data.Text                        as T
import           GHC.Generics                        (Generic, Rep)
import           GHC.Stack
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Client              as HTTP
import           Prelude.Unicode
import           Servant.API
import           Servant.Client
import qualified System.Logger                    as Log


-- * Local imports
--
import           Types


-- | https://bittrex.com/api/{version}/{method}?param=value
bittrexURL ∷  BaseUrl
bittrexURL = (BaseUrl Https "bittrex.com" 443 "/api/v1.1")

options ∷ Options
options = Options $ Log.defSettings
                  & Log.setLogLevel Log.Trace

trace ∷ Bool
trace = True

check ∷ Text → IO ()
check uri = do
  logger ← Log.new $ o'logging options
  manager ← newTlsManagerWith tlsManagerSettings
            { HTTP.managerModifyRequest =
              (\r → (Log.trace logger (Log.msg $ showT r)) >> pure r)
            , HTTP.managerModifyResponse =
              (\r → do
                  (Log.trace logger (Log.msg $ showT $ HTTP.responseStatus r))
                  (Log.trace logger (Log.msg $ showT $ HTTP.responseHeaders r))
                  (Log.flush logger)
                  -- body ← HTTP.responseBody r
                  -- (Log.trace logger (Log.msg $ showT $ body))
                  pure r) }
  initReq ← HTTP.parseRequest $ T.unpack uri
  bs ← flip HTTP.httpLbs manager
       $ initReq { HTTP.method = "GET", HTTP.port = 443, HTTP.secure = True
                 , HTTP.requestHeaders = [("Accept", "application/json;charset=utf-8,application/json")] }
  putStrLn $ show bs
  pure ()

run ∷ HasCallStack ⇒ ClientM a → IO a
run action = do
  logger ← Log.new $ o'logging options
  manager ← newTlsManagerWith tlsManagerSettings
            { HTTP.managerModifyRequest =
              (\r → do
                  (Log.trace logger (Log.msg $ showT r))
                  (Log.flush logger)
                  pure r)
            , HTTP.managerModifyResponse =
              (\r → do
                  -- (Log.trace logger (Log.msg $ showT $ HTTP.responseStatus r))
                  -- (Log.trace logger (Log.msg $ showT $ HTTP.responseHeaders r))
                  -- (Log.flush logger)
                  -- body ← HTTP.responseBody r
                  -- (Log.trace logger (Log.msg $ showT $ body))
                  pure r)
            }
  let conn = ClientEnv manager bittrexURL
  res ← (flip runClientM conn -- ∘ flip runReaderT logger
        ) $ do
    -- lift
    action
  case res of
    Left err → error $ "Error: " <> show err
    Right x  → pure x

type BittrexAPI =
          "public/getmarkets"
          :> Get '[JSON] (Response [DescMarket])
     :<|> "public/getcurrencies"
          :> Get '[JSON] (Response [DescCurrency])
     :<|> "public/getticker"
          :> QueryParam "market" A'Pair
          :> Get '[JSON] (Response  DescTicker)                 -- BTC-LTC
     :<|> "public/getmarketsummaries"
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getmarketsummary"
          :> QueryParam "market" A'Pair
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getorderbook"
          :> QueryParam "market" A'Pair
          :> QueryParam "type"   OrderBookType
          :> Get '[JSON] (Response  DescOrderBook)
     :<|> "public/getmarkethistory"
          :> QueryParam "market" A'Pair
          :> Get '[JSON] (Response [DescMarketHistoryPoint])

bittrexAPI ∷ Proxy BittrexAPI
bittrexAPI = Proxy

getmarkets, getcurrencies, getticker, getmarketsummaries, getmarketsummary, getorderbook, getmarkethistory ∷ _
getmarkets           :<|>
  getcurrencies      :<|>
  getticker          :<|>
  getmarketsummaries :<|>
  getmarketsummary   :<|>
  getorderbook       :<|>
  getmarkethistory
  =
  client bittrexAPI


-- * API data structures
--
data Response a where
  Response ∷
    { success ∷ Bool
    , message ∷ Text
    , result  ∷ Maybe a
    } → Response a
    deriving (Generic, Show)
instance FromJSON a ⇒ FromJSON (Response a)

data DescMarket where
  DescMarket ∷
    { maMarketCurrency     ∷ Sym
    , maBaseCurrency       ∷ Sym
    , maMarketCurrencyLong ∷ Text
    , maBaseCurrencyLong   ∷ Text
    , maMinTradeSize       ∷ Double
    , maMarketName         ∷ Text
    , maIsActive           ∷ Bool
    , maCreated            ∷ Text --UTCTime
    } → DescMarket
    deriving (Show, Generic)
instance FromJSON DescMarket where parseJSON = dropPrefix "ma"

data DescCurrency where
  DescCurrency ∷
    { cuCurrency           ∷ Text
    , cuCurrencyLong       ∷ Text
    , cuMinConfirmation    ∷ Int
    , cuTxFee              ∷ Double
    , cuIsActive           ∷ Bool
    , cuCoinType           ∷ Text --CoinType
    , cuBaseAddress        ∷ Maybe Text
    } → DescCurrency
    deriving (Show, Generic)
instance FromJSON DescCurrency where parseJSON = dropPrefix "cu"

data DescTicker where
  DescTicker ∷
    { tiBid                ∷ Double
    , tiAsk                ∷ Double
    , tiLast               ∷ Double
    } → DescTicker
    deriving (Show, Generic)
instance FromJSON DescTicker where parseJSON = dropPrefix "ti"

data DescMarketSummary where
  DescMarketSummary ∷
    { msMarketName         ∷ Text
    , msHigh               ∷ Double
    , msLow                ∷ Double
    , msVolume             ∷ Double
    , msLast               ∷ Double
    , msBaseVolume         ∷ Double
    , msTimeStamp          ∷ Text --UTCTime
    , msBid                ∷ Double
    , msAsk                ∷ Double
    , msOpenBuyOrders      ∷ Int
    , msOpenSellOrders     ∷ Int
    , msPrevDay            ∷ Double
    , msCreated            ∷ Text --UTCTime
    , msDisplayMarketName  ∷ Maybe Text
    } → DescMarketSummary
    deriving (Show, Generic)
instance FromJSON DescMarketSummary where parseJSON = dropPrefix "ms"

data DescPosition where
  DescPosition ∷
    { poQuantity           ∷ Double
    , poRate               ∷ Double
    } → DescPosition
    deriving (Show, Generic)
instance FromJSON DescPosition where parseJSON = dropPrefix "po"

data DescOrderBook where
  DescOrderBook ∷
    { obbuy                ∷ [DescPosition]
    , obsell               ∷ [DescPosition]
    } → DescOrderBook
    deriving (Show, Generic)
instance FromJSON DescOrderBook where parseJSON = dropPrefix "ob"

data DescMarketHistoryPoint where
  DescMarketHistoryPoint ∷
    { mhId                 ∷ Integer
    , mhTimeStamp          ∷ Text --UTCTime
    , mhQuantity           ∷ Double
    , mhPrice              ∷ Double
    , mhTotal              ∷ Double
    , mhFillType           ∷ FillType
    , mhOrderType          ∷ Direction
    } → DescMarketHistoryPoint
    deriving (Show, Generic)
instance FromJSON DescMarketHistoryPoint where parseJSON = dropPrefix "mh"


dropPrefix ∷ (Generic a, GFromJSON Zero (Rep a)) ⇒ String → Value → Parser a
dropPrefix x =
  genericParseJSON $ defaultOptions { fieldLabelModifier =
                                      (stripPrefix x
                                        >>> fromMaybe (error "valiant stripPrefix error")) }

instance ToHttpApiData OrderBookType where
  toUrlPiece   = T.drop 2 ∘ lowerShowT
  toQueryParam = T.drop 2 ∘ lowerShowT

instance ToHttpApiData A'Pair where
  toUrlPiece   = T.toLower ∘ pp
  toQueryParam = T.toLower ∘ pp
