{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Harum
where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                    as      T
import qualified Data.Text.IO                 as      T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX               (posixSecondsToUTCTime)
import           Data.Time.LocalTime                 (LocalTime (..), utcToLocalTime, hoursToTimeZone)
import           GHC.Generics                 hiding (C)
import           Network.HTTP.Client.TLS      hiding (Proxy, Response, path)
import qualified Network.HTTP.Client          as      HTTP
import qualified Network.HTTP.Client.TLS      as      HTTP
import           Prelude.Unicode
import           Servant.API
import           Servant.Client
import           Text.Printf


-- * UI imports

-- import qualified Graphics.Vty                     as VT
-- import           UI


lowerShowT ∷ Show a ⇒ a → Text
lowerShowT = T.pack ∘ (toLower <$>) ∘ show


data CoinType
  = BITCOIN
  deriving (Show, Generic)

data Sym
  = BTC
  | BCC
  | ETH
  | USDT
  deriving (Show, Generic)

data Act
  = BUY
  | SELL
  deriving (Show, Generic)

data Cu (a ∷ Sym) where
  Btc  ∷ Cu BTC
  Bcc  ∷ Cu BCC
  Eth  ∷ Cu ETH
  Usdt ∷ Cu USDT
deriving instance Show (Cu a)

data ACu where
  ACu ∷ { fromACu ∷ Cu a } → ACu

data Op (a ∷ Act) where
  Buy  ∷ Op BUY
  Sell ∷ Op SELL

data FillType where
  Fill        ∷ FillType
  PartialFill ∷ FillType
  deriving (Generic, Show)


data Market where
  Market ∷ Sym → Sym → Market
  deriving (Generic, Show)

instance ToHttpApiData Market where
  toUrlPiece   (Market b t) = lowerShowT b <> "-" <> lowerShowT t
  toQueryParam (Market b t) = lowerShowT b <> "-" <> lowerShowT t

data OrderBookType
  = OBBuy
  | OBSell
  | OBBoth
  deriving (Show, Generic)

instance ToHttpApiData OrderBookType where
  toUrlPiece   = T.drop 2 ∘ lowerShowT
  toQueryParam = T.drop 2 ∘ lowerShowT


-- | https://bittrex.com/api/{version}/{method}?param=value
bittrexURL = (BaseUrl Https "bittrex.com" 443 "/api/v1.1")

data Response a where
  Response ∷
    { success ∷ Bool
    , message ∷ Text
    , result  ∷ a
    } → Response a
    deriving (Generic, Show)
instance FromJSON a ⇒ FromJSON (Response a)

type BittrexAPI =
          "public/getmarkets"
          :> Get '[JSON] (Response [DescMarket])
     :<|> "public/getcurrencies"
          :> Get '[JSON] (Response [DescCurrency])
     :<|> "public/getticker"
          :> QueryParam "market" Market
          :> Get '[JSON] (Response  DescTicker)                 -- BTC-LTC
     :<|> "public/getmarketsummaries"
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getmarketsummary"
          :> QueryParam "market" Market
          :> Get '[JSON] (Response [DescMarketSummary])
     :<|> "public/getorderbook"
          :> QueryParam "market" Market
          :> QueryParam "depth"  Int
          :> QueryParam "type"   OrderBookType
          :> Get '[JSON] (Response  DescOrderBook)
     :<|> "public/getmarkethistory"
          :> QueryParam "market" Market
          :> Get '[JSON] (Response [DescMarketHistoryPoint])

bittrexAPI ∷ Proxy BittrexAPI
bittrexAPI = Proxy

getmarkets           :<|>
  getcurrencies      :<|>
  getticker          :<|>
  getmarketsummaries :<|>
  getmarketsummary   :<|>
  getorderbook       :<|>
  getmarkethistory
  =
  client bittrexAPI

-- OAuth: http://mittrasw.blogspot.ru/2016/05/oauth-10a-for-etrade-api-using-haskell.html
run ∷ IO ()
run = do
  -- HTTP.managerModifyRequest (\r → do
  --                               pure r)
  manager ← newTlsManagerWith 
            tlsManagerSettings
            { HTTP.managerModifyRequest =
                (\r → do
                    -- print r
                    pure r) }
  res     ← flip runClientM (ClientEnv manager bittrexURL) $
            getticker (Just $ Market USDT BTC)
  case res of
    Left err → putStrLn $ "Error: " <> show err
    Right Response{..}  → do
      print success
      print message
      print result

-- $apikey='xxx';
-- $apisecret='xxx';
-- $nonce=time();
-- $uri='https://bittrex.com/api/v1.1/market/getopenorders?apikey='.$apikey.'&nonce='.$nonce;
-- $sign=hash_hmac('sha512',$uri,$apisecret);
-- $ch = curl_init($uri);
-- curl_setopt($ch, CURLOPT_HTTPHEADER, array('apisign:'.$sign));
-- $execResult = curl_exec($ch);
-- $obj = json_decode($execResult);


instance FromJSON Sym
instance FromJSON Act
instance FromJSON CoinType
instance FromJSON FillType

dropPrefix ∷ (Generic a, GFromJSON Zero (Rep a)) ⇒ String → Value → Parser a
dropPrefix x = genericParseJSON $ defaultOptions { fieldLabelModifier = (stripPrefix x >>> fromMaybe (error "valiant stripPrefix error")) }

data DescMarket where
  DescMarket ∷
    { maCurrency           ∷ Sym
    , maBaseCurrency       ∷ Sym
    , maCurrencyLong       ∷ Text
    , maBaseCurrencyLong   ∷ Text
    , maMinTradeSize       ∷ Double
    , maName               ∷ Text
    , maIsActive           ∷ Bool
    , maCreated            ∷ UTCTime
    } → DescMarket
    deriving (Show, Generic)
instance FromJSON DescMarket where parseJSON = dropPrefix "ma"

data DescCurrency where
  DescCurrency ∷
    { cuCurrency           ∷ Sym 
    , cuCurrencyLong       ∷ Text
    , cuMinConfirmation    ∷ Int
    , cuTxFee              ∷ Double
    , cuIsActive           ∷ Bool
    , cuCoinType           ∷ CoinType
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
    { msName               ∷ Text
    , msHigh               ∷ Double
    , msLow                ∷ Double
    , msVolume             ∷ Double
    , msLast               ∷ Double
    , msBaseVolume         ∷ Double
    , msTimeStamp          ∷ UTCTime
    , msBid                ∷ Double
    , msAsk                ∷ Double
    , msOpenBuyOrders      ∷ Int
    , msOpenSellOrders     ∷ Int
    , msPrevDay            ∷ Double
    , msCreated            ∷ UTCTime
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
    , mhTimestamp          ∷ UTCTime
    , mhQuantity           ∷ Double
    , mhPrice              ∷ Double
    , mhTotal              ∷ Double
    , mhFillType           ∷ FillType
    , mhOrderType          ∷ Act
    } → DescMarketHistoryPoint
    deriving (Show, Generic)
instance FromJSON DescMarketHistoryPoint where parseJSON = dropPrefix "mh"

-- data Desc where
--   Desc ∷
--     {  ∷ 
--     ,  ∷ 
--     } → Desc


ppACu ∷ ACu → String
ppACu (ACu x) = toUpper <$> show x


data Val a where
  Val ∷ Cu a → Float → Val a
deriving instance Show (Val a)


data Rate t where
  Rate ∷ Cu a → Cu b → Float → Rate '(a, b)
deriving instance Show (Rate a)

rRate ∷ Rate a → Float
rRate (Rate _ _ r) = r

ppRate ∷ Rate a → String
ppRate (Rate c0 c1 r) = printf "%s %s %f" (show c0) (show c1) r

inverse ∷  Rate '(a, b) → Rate '(b, a)
inverse    (Rate a b r) =   Rate b a (1 / r)
compose ∷  Rate '(a, b) → Rate '(b, c)  →  Rate '(a, c)
compose    (Rate a _ ab)   (Rate _ c bc)    = Rate a c (ab * bc)


data Route (r ∷ [(Sym, Sym)]) where
  R     ∷ Rate '(a, b) → Route '[ '(a, b)]
  (:-:) ∷ Rate '(a, b) → Route ('(b, c) : d) → Route ('(a, b) : '(b, c) : d)
deriving instance Show (Route a)

infixr :-:

type family Reduce x where
  Reduce ('(a, b) : '[])         = '(a, b)
  Reduce ('(a, b) : '(b, c) : d) = Reduce ('(a, c) : d)

describe ∷ Route a → [ACu]
describe (R (Rate a b _)) = [ACu a, ACu b]
describe ((Rate a _ _) :-: x) = ACu a : describe x

reduce ∷ Route a → Rate (Reduce a)
reduce        (R r)         = r
reduce (r1 :-: R r2)        = compose r1 r2
reduce (r1 :-: (r2 :-: r3)) = reduce ((compose r1 r2) :-: r3)

data Path where
  Path ∷ Route a → Rate (Reduce a) → Path
deriving instance Show Path

path ∷ Route a → Path
path route = Path route (reduce route)


type family Dst (op ∷ Act) (a ∷ Sym) (b ∷ Sym) ∷ Sym where
  Dst BUY  a b = b
  Dst SELL a b = a
type family Src (op ∷ Act) (a ∷ Sym) (b ∷ Sym) ∷ Sym where
  Src BUY  a b = a
  Src SELL a b = b

exec ∷ Val (Src op a b) → Op op → Rate '(a, b)  →  Val (Dst op a b)
exec   (Val _ v)            Buy     (Rate a b r) = Val b (v / r)
exec   (Val _ v)            Sell    (Rate a b r) = Val a (v * r)


-- Rate Usd Btc 2673.0
-- Rate Usd Eth 200.182
-- Rate Btc Eth 0.07533684

paths ∷ Rate '(USDT, BTC) → Rate '(USDT, BCC) → Rate '(USDT, ETH) → Rate '(BTC, ETH) → Rate '(BTC, BCC) → [Path]
paths usd'btc usd'bcc usd'eth btc'eth btc'bcc  =
  let btc'usd = inverse usd'btc
      bcc'usd = inverse usd'bcc
      eth'btc = inverse btc'eth
      eth'usd = inverse usd'eth
      bcc'btc = inverse btc'bcc
  in
  [ path $ R usd'btc
  , path $ usd'bcc :-: R bcc'btc
  , path $ usd'bcc :-:   bcc'btc :-: R btc'usd
  , path $ usd'btc :-:   btc'bcc :-: R bcc'usd
  , path $ R bcc'usd
  , path $ bcc'btc :-: R btc'usd
  , path $ bcc'btc :-:   btc'usd :-: R usd'bcc
  , path $ bcc'usd :-:   usd'btc :-: R btc'bcc
  , path $ R btc'bcc
  , path $ btc'usd :-: R usd'bcc
  , path $ btc'usd :-:   usd'bcc :-: R bcc'btc
  , path $ btc'bcc :-:   bcc'usd :-: R usd'btc
  , path $ R bcc'btc
  , path $ bcc'usd :-: R usd'btc
  , path $ bcc'usd :-:   usd'btc :-: R btc'bcc
  , path $ R btc'usd
  , path $ btc'bcc :-: R bcc'usd
  , path $ btc'bcc :-:   bcc'usd :-: R usd'btc
  --
  -- , path $ usd'eth :-: R eth'btc
  -- , path $ R usd'eth
  -- , path $ usd'btc :-: R btc'eth
  -- , path $ R btc'eth
  -- , path $ btc'usd :-: R usd'eth
  -- , path $ btc'eth :-: R eth'usd
  -- , path $ R eth'usd
  -- , path $ eth'btc :-: R btc'usd
  ]

ppp ∷ Path → IO ()
ppp (Path route rate) =
  printf "%20s: %s\n" (intercalate "→" $ (ppACu <$> describe route)) (show (rRate rate))

ppr ∷ forall a . Route a → IO ()
ppr route = ppp $ path route 

pp ∷ Rate '(USDT, BTC) → Rate '(USDT, BCC) → Rate '(BTC, BCC) → Rate '(USDT, ETH) → Rate '(BTC, ETH) → IO ()
pp ub uc bc ue be =
  mapM_ ppp (paths ub uc ue be bc)

main ∷ IO ()
main = do
  pp
    (Rate Usdt Btc 2663)
    (Rate Usdt Bcc 700)
    (Rate Btc  Bcc 0.265)
    (Rate Usdt Eth 217)
    (Rate Btc  Eth 0.0810)
-- main = do
--   let g = molecule "Available work" (VT.KBackTab, VT.KChar '\t') False
--           [ (APt WTExpr,       AtmF (derive_atom (Name "<inputbar>")      ∷ FEditor))
--           , (APt WTScreenTabs, AtmF (derive_atom def                      ∷ STabs))
--           , (APt WTScreen,     AtmF (derive_atom def                      ∷ Screen))
--           ]
--   _ ← UI.run $
--       case yt_failure of
--         Nothing → [g]
--         -- XXX: message story is broken
--         Just e  → (:[]) ∘ molecule_set_popup "Error" g -- XXX: how to automatically reflow text in brick?
--                   $  [ " "
--                      , "  Continuable error while querying YouTrack server for issues:"
--                      , " " ]
--                   <> (concat ∘ map (chunksOf 80) ∘ lines $ show e)
--                   <> [ " "
--                      , "  Esc to continue with fake YT issues.."
--                      , " " ]
--   pure ()
