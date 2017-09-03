{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-partial-type-signatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Bittrex
where

import           Control.Arrow                       ((>>>))
import           Control.Monad                       (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control      as CMTC
import qualified Data.Aeson                       as AE
import           Data.Aeson                   hiding (pairs)
import           Data.Aeson.Types             hiding (Options, Pair, pairs)
import qualified Data.ByteString.Lazy             as BL
-- import qualified Data.ByteString                  as BS
import           Data.Char                           (toLower)
import           Data.Function                       ((&))
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Proxy                          (Proxy(..))
import           Data.String
import           Data.Text                           (Text)
import           Data.Foldable                       (asum)
import           Data.List                           (stripPrefix)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           GHC.Generics                        (Generic, Rep)
import           GHC.Stack
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Client              as HTTP
import qualified Network.WebSockets               as WS
import           Prelude.Unicode
import           Servant.API
import           Servant.Client
import qualified System.Logger                    as Log
import           Text.Printf                         (printf)
import           Time.System
import           Time.Types
import qualified Wuss                             as WS


-- * Local imports
--
import           Types


-- | https://bittrex.com/api/{version}/{method}?param=value
bittrexHTTPURL, bittrexSignalrURL ∷  BaseUrl
bittrexHTTPURL    = (BaseUrl Https "bittrex.com"        443 "/api/v1.1")
bittrexSignalrURL = (BaseUrl Https "socket.bittrex.com" 443 "/signalr")
options    ∷ Options
options    = Options $ Log.defSettings & Log.setLogLevel Log.Trace

toJSONLowerStrip ∷ (Generic a, GToJSON Zero (Rep a)) ⇒ Int → a → Value
toJSONLowerStrip n = genericToJSON $ defaultOptions { fieldLabelModifier = map toLower . drop n }

fromJSONStrip ∷ (Generic a, AE.GFromJSON AE.Zero (Rep a)) ⇒ Int → Value → Parser a
fromJSONStrip n = AE.genericParseJSON (AE.defaultOptions { fieldLabelModifier = drop n })

newtype SignalrHub = SignalrHub { fromSH ∷ Text } deriving (Generic, IsString, Show, ToJSON)
data CDE
  =  CDE
     { cdName ∷ SignalrHub
     } deriving (Generic, Show)
instance ToJSON CDE where toJSON = toJSONLowerStrip 2
instance ToHttpApiData [CDE] where
  toUrlPiece   = T.decodeUtf8 ∘ BL.toStrict ∘ AE.encode
  toQueryParam = T.decodeUtf8 ∘ BL.toStrict ∘ AE.encode

newtype ConnectionToken = ConnectionToken { fromCT ∷ Text } deriving (Eq, FromJSON, ToJSON, Show, ToHttpApiData, IsString)

type BittrexSignalr =
          "negotiate"
          :> QueryParam "clientProtocol"  Text
          :> QueryParam "connectionData"  [CDE]
          :> QueryParam "_"               Integer
          :> Get '[JSON] NegotiateR
     :<|> "start"
          :> QueryParam "transport"       Text
          :> QueryParam "clientProtocol"  Text
          :> QueryParam "connectionToken" ConnectionToken
          :> QueryParam "connectionData"  [CDE]
          :> QueryParam "_"               Integer
          :> Get '[JSON] StartR

data NegotiateR where
  NegotiateR ∷
    { rnUrl ∷ Text
    , rnConnectionToken ∷ ConnectionToken -- "827mUg7d5uEvxNY85ggM8FKArVDowHTNtL5+4BfpTkVIL3TpvXoVeSz25uDcqwSsEywLOPXdod2BhRgsuBMRpGNmgATS76CWoZjSuisXVKzIVDEg"
    , rnConnectionId ∷ Text -- "0b4dc066-69cb-4685-b26d-0e4968d1a629"
    , rnKeepAliveTimeout ∷ Float
    , rnDisconnectTimeout ∷ Float
    , rnConnectionTimeout ∷ Float
    , rnTryWebSockets ∷ Bool
    , rnProtocolVersion ∷ Text
    , rnTransportConnectTimeout ∷ Float
    , rnLongPollDelay ∷ Float
    } → NegotiateR
    deriving (Generic, Show)
instance FromJSON NegotiateR where parseJSON = fromJSONStrip 2

data StartR where
  StartR ∷
    { rcResponse ∷ Text
    } → StartR
    deriving (Generic, Show)
instance FromJSON StartR where parseJSON = fromJSONStrip 2

bittrexSignalr ∷ Proxy BittrexSignalr
bittrexSignalr = Proxy
negotiate, start ∷ _
negotiate           :<|>
  start
  =
  client bittrexSignalr

-- withFile :: FilePath -> Int -> (String -> IO r) -> IO r
-- withFile = (⊥)
-- withFileLifted' :: (Monad (t IO), MonadTransControl t) => FilePath -> Int -> (String -> t IO r) -> t IO r
-- withFileLifted' file mode action = liftWith ((\run -> withFile file mode (run . action))∷_) >>= restoreT . return


step0 ∷ IO NegotiateR
step0 = runSignalr $ do
  (Elapsed unixTime) ← liftIO timeCurrent
  let cdata    = [CDE { cdName = "corehub" }]
      protoVer = "1.5"
  n@NegotiateR{..} ← negotiate
    (Just protoVer) (Just cdata) (Just $ fromIntegral unixTime)
  (Elapsed unixTime') ← liftIO timeCurrent
  -- unless (rcResponse ≡ "started") $
  --   errorT $ "Remote response != 'started', but is: '" <> rcResponse <> "'"
  let wss'opts = WS.defaultConnectionOptions
      wss'ep   = printf "/signalr/connect?transport=webSockets&clientProtocol=%s&connectionToken=%s&connectionData=%s&tid=1"
                 protoVer (T.unpack $ fromCT rnConnectionToken) (T.decodeUtf8 $ BL.toStrict $ AE.encode cdata)
  void ∘ CMTC.restoreM ∘ return =<< CMTC.liftBaseWith
    (\run →
       WS.runSecureClientWith "socket.bittrex.com" 443 wss'ep wss'opts [] $
       \connection →
         let loop = do
               x ← WS.receive connection
               putStrLn $ show x
               loop
         in do
           r ← run $ start
               (Just "webSockets") (Just protoVer) (Just rnConnectionToken) (Just cdata) (Just $ fromIntegral unixTime')
           case r of
             Right StartR{..} → loop
             Left err → errorT $ showT err)
  pure n
-- wss://socket.bittrex.com/signalr/connect?transport=webSockets&clientProtocol=1.5&connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j&connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D&tid=6
-- - signalr/connect
-- - transport=webSockets
-- - clientProtocol=1.5
-- - connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j
-- - connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D
-- - tid=6

get ∷ Text → IO BL.ByteString
get uri = do
  logger ← Log.new $ o'logging options
  manager ← newTlsManagerWith tlsManagerSettings
            { HTTP.managerModifyRequest =
              (\r → (Log.trace logger (Log.msg $ showT r)) >> pure r)
            , HTTP.managerModifyResponse =
              (\r → do
                  Log.trace logger (Log.msg $ showT $ HTTP.responseStatus r)
                  Log.trace logger (Log.msg $ showT $ HTTP.responseHeaders r)
                  pure r) }
  initReq ← HTTP.parseRequest $ T.unpack uri
  r ← flip HTTP.httpLbs manager
      $ initReq { HTTP.method = "GET", HTTP.port = 443, HTTP.secure = True
                , HTTP.requestHeaders = [("Accept", "application/json;charset=utf-8,application/json")] }
  Log.trace logger (Log.msg $ showT $ HTTP.responseBody r)
  Log.flush logger
  pure $ HTTP.responseBody r

decode ∷ (HasCallStack, FromJSON r) ⇒ BL.ByteString → r
decode bs = do
  case eitherDecode bs of
    Left e     → error e
    Right resp → resp

runHTTP, runSignalr ∷ HasCallStack ⇒ ClientM a → IO a
runHTTP    = run' bittrexHTTPURL
runSignalr = run' bittrexSignalrURL

run' ∷ HasCallStack ⇒ BaseUrl → ClientM a → IO a
run' baseurl action = do
  logger ← Log.new $ o'logging options
  manager ← newTlsManagerWith tlsManagerSettings
            { HTTP.managerModifyRequest =
              (\r → do
                  Log.trace logger (Log.msg $ showT r)
                  Log.flush logger
                  pure r)
            , HTTP.managerModifyResponse =
              (\r → do
                  Log.trace logger (Log.msg $ showT $ HTTP.responseStatus r)
                  Log.trace logger (Log.msg $ showT $ HTTP.responseHeaders r)
                  Log.flush logger
                  pure r)
            }
  let conn = ClientEnv manager baseurl
  res ← (flip runClientM conn -- ∘ flip runReaderT logger
        ) $ do
    -- lift
    action
  case res of
    Left err → error $ "Error: " <> show err
    Right x  → pure x

type BittrexHTTP =
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

bittrexHTTP ∷ Proxy BittrexHTTP
bittrexHTTP = Proxy

getmarkets, getcurrencies, getticker, getmarketsummaries, getmarketsummary, getorderbook, getmarkethistory ∷ _
getmarkets           :<|>
  getcurrencies      :<|>
  getticker          :<|>
  getmarketsummaries :<|>
  getmarketsummary   :<|>
  getorderbook       :<|>
  getmarkethistory
  =
  client bittrexHTTP


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
    , mhOrderType          ∷ Dir
    } → DescMarketHistoryPoint
    deriving (Show, Generic)
instance FromJSON DescMarketHistoryPoint where parseJSON = dropPrefix "mh"


-- * Extras for bindings
--
data FillType where
  Fill        ∷ FillType
  PartialFill ∷ FillType
  deriving (Generic, Show)

data OrderBookType
  = OBBuy
  | OBSell
  | OBBoth
  deriving (Show, Generic)

instance FromJSON Sym
instance FromJSON Dir where
  parseJSON = withObject "direction" $
    \_ → asum [ pure Bid, pure Ask ]
instance FromJSON FillType


dropPrefix ∷ (Generic a, GFromJSON Zero (Rep a)) ⇒ String → Value → Parser a
dropPrefix x =
  genericParseJSON $ defaultOptions { fieldLabelModifier =
                                      (stripPrefix x
                                        >>> fromMaybe (error "valiant stripPrefix error")) }

instance ToHttpApiData OrderBookType where
  toUrlPiece   = T.drop 2 ∘ lowerShowT
  toQueryParam = T.drop 2 ∘ lowerShowT

instance ToHttpApiData A'Pair where
  toUrlPiece   = pp
  toQueryParam = pp
  -- toUrlPiece   = T.toLower ∘ pp
  -- toQueryParam = T.toLower ∘ pp
