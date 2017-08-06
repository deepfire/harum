{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Bittrex
where

import           Control.Arrow                       ((>>>))
import           Data.Aeson                   hiding (pairs)
import           Data.Aeson.Types             hiding (Pair, pairs)
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Proxy                          (Proxy(..))
import           Data.Text                           (Text)
import           Data.List                           (stripPrefix)
import qualified Data.Text                        as T
import           GHC.Generics                        (Generic, Rep)
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Client              as HTTP
import           Prelude.Unicode
import           Servant.API
import           Servant.Client


-- * Local imports
--
import           Types


-- | https://bittrex.com/api/{version}/{method}?param=value
bittrexURL ∷  BaseUrl
bittrexURL = (BaseUrl Https "bittrex.com" 443 "/api/v1.1")

run ∷ ClientM a → IO ()
run action = do
  manager ← newTlsManagerWith tlsManagerSettings { HTTP.managerModifyRequest = (\r → pure r) }
  let conn = ClientEnv manager bittrexURL
  res ← flip runClientM conn action
  case res of
    Left err → putStrLn $ "Error: " <> show err
    Right _  → pure ()

type BittrexAPI =
          "public/getmarkets"
          :> Get '[JSON] (Response [DescMarket])
     :<|> "public/getcurrencies"
          :> Get '[JSON] (Response [DescCurrency])
     :<|> "public/getticker"
          :> QueryParam "market" Syms
          :> Get '[JSON] (Response  DescTicker)                 -- BTC-LTC
     :<|> "public/getmarketsummaries"
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getmarketsummary"
          :> QueryParam "market" Syms
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getorderbook"
          :> QueryParam "market" Syms
          :> QueryParam "type"   OrderBookType
          :> Get '[JSON] (Response  DescOrderBook)
     :<|> "public/getmarkethistory"
          :> QueryParam "market" Syms
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
    , result  ∷ a
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

instance ToHttpApiData Syms where
  toUrlPiece   (Syms b t) = lowerShowT b <> "-" <> lowerShowT t
  toQueryParam (Syms b t) = lowerShowT b <> "-" <> lowerShowT t
