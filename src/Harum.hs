{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Harum
where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                    hiding (lines)
import           Data.Maybe
import           Data.Monoid                  hiding (Last)
import           Data.Text                           (Text)
import qualified Data.Text                    as      T
import qualified Data.Text.IO                 as      T
import           Network.HTTP.Client.TLS
import qualified Network.HTTP.Client          as      HTTP
import           Prelude                      hiding (lines)
import           Prelude.Unicode
import           Servant.Client
import           Safe
import           Text.Printf


-- * Local imports
--
import           Types
import           Bittrex
import           GenericClient
-- import           UI


-- * A stab at analytics

-- analyse'triangle ∷ Market a b → Market b c → Rate Median '(a, c)
-- analyse'triangle left right =
--   market'median left `compose'medians` market'median right


-- * HUD

trade'hud ∷ Int → Trade → [Text]
trade'hud depth (Trade m@Market{..} Book{..}) =
  let Syms{..}     = syms m
      info         = T.pack $ printf ". %-*.7f" (int 11) (fromRate mk'last)
      pp'order Order{..}
                   = T.pack $ printf "%-*s %*.7f" (int 14) (brief rate) (int 11) volume
      asks'        = take depth (pp'order <$> asks)
      bids'        = take depth (pp'order <$> bids)
      width        = fromMaybe 22 $ (T.length <$> headMay asks')
      head'left    = show s'base
      head'right   = show s'market
      header       = T.pack $ printf "%-*s%*s" (width `div` 2 + width `mod` 2) head'left (width `div` 2) head'right
  in [    header ]
     <>   reverse asks'
     <> [ info ]
     <>           bids'

base'pairs, extra'pairs, hud'pairs ∷ [APair]
base'pairs =
  [ PA $ Pair Usdt Btc
  , PA $ Pair Usdt Bcc
  ]
extra'pairs =
  [
    PA $ Pair Btc  Bcc
  ]
hud'pairs =
  base'pairs <> extra'pairs

hud'frame ∷ ClientM ()
hud'frame = do
  trades ← sequence [ get'trade 20 pa
                    | PA pa ← hud'pairs ]
  let huds      = trade'hud 10 <$> trades
      hud'ws    = (maximum ∘ (T.length <$>)) <$> huds
      hud'lines = transpose huds
      hud       = [ T.concat ∘ intersperse "  │  "
                    $ (uncurry justify <$> zip widths lines)
                  | (widths, lines) ← zip (repeat hud'ws) hud'lines]
      justify w s = T.pack $ printf "%-*s" w s
  liftIO $ forM_ hud T.putStrLn

run ∷ IO ()
run = do
  manager ← newTlsManagerWith tlsManagerSettings { HTTP.managerModifyRequest = (\r → pure r) }
  let conn = ClientEnv manager bittrexURL
  res     ← flip runClientM conn
            hud'frame
  case res of
    Left err → putStrLn $ "Error: " <> show err
    Right _  → pure ()

  -- xs ← handlerr $ getcurrencies
  -- forM_ xs $
  --   \DescCurrency{..}→
  --     liftIO $ printf "| %s\n" cuCurrency
  -- xs ← handlerr $ getmarkets
  -- forM_ xs $
  --   \DescMarket{..}→
  --     liftIO $ printf "%s→%s:\n" (show maBaseCurrency) (show maMarketCurrency)

-- OAuth: http://mittrasw.blogspot.ru/2016/05/oauth-10a-for-etrade-api-using-haskell.html
-- $apikey='xxx';
-- $apisecret='xxx';
-- $nonce=time();
-- $uri='https://bittrex.com/api/v1.1/market/getopenorders?apikey='.$apikey.'&nonce='.$nonce;
-- $sign=hash_hmac('sha512',$uri,$apisecret);
-- $ch = curl_init($uri);
-- curl_setopt($ch, CURLOPT_HTTPHEADER, array('apisign:'.$sign));
-- $execResult = curl_exec($ch);
-- $obj = json_decode($execResult);


-- data Route (r ∷ [(Sym, Sym)]) where
--   R     ∷ Rate k '(a, b) → Route '[ '(a, b)]
--   (:-:) ∷ Rate k '(a, b) → Route ('(b, c) : d) → Route ('(a, b) : '(b, c) : d)
-- deriving instance Show (Route a)

-- infixr :-:

-- type family Reduce x where
--   Reduce ('(a, b) : '[])         = '(a, b)
--   Reduce ('(a, b) : '(b, c) : d) = Reduce ('(a, c) : d)

-- describe ∷ Route a → [ACu]
-- describe (R (Rate k a b _)) = [ACu a, ACu b]
-- describe ((Rate k a _ _) :-: x) = ACu a : describe x

-- reduce ∷ Route a → Rate k (Reduce a)
-- reduce        (R r)         = r
-- reduce (r1 :-: R r2)        = compose r1 r2
-- reduce (r1 :-: (r2 :-: r3)) = reduce ((compose r1 r2) :-: r3)

-- data Path where
--   Path ∷ Route a → Rate k (Reduce a) → Path
-- deriving instance Show Path

-- path ∷ Route a → Path
-- path route = Path route (reduce route)


-- type family Dst (op ∷ Action) (a ∷ Sym) (b ∷ Sym) ∷ Sym where
--   Dst BUY  a b = b
--   Dst SELL a b = a
-- type family Src (op ∷ Action) (a ∷ Sym) (b ∷ Sym) ∷ Sym where
--   Src BUY  a b = a
--   Src SELL a b = b

-- exec ∷ Val (Src op a b) → Act act → Rate k '(a, b) →  Val (Dst op a b)
-- exec  (Val _ v)           Buy      (Rate k a b r)   = Val b (v / r)
-- exec  (Val _ v)           Sell     (Rate k a b r)   = Val a (v * r)


-- paths ∷ Rate k '(USDT, BTC) → Rate k '(USDT, BCC) → Rate k '(USDT, ETH) → Rate k '(BTC, ETH) → Rate k '(BTC, BCC) → [Path]
-- paths usd'btc usd'bcc usd'eth btc'eth btc'bcc  =
--   let btc'usd = inverse usd'btc
--       bcc'usd = inverse usd'bcc
--       eth'btc = inverse btc'eth
--       eth'usd = inverse usd'eth
--       bcc'btc = inverse btc'bcc
--   in
--   [ path $ R usd'btc
--   , path $ usd'bcc :-: R bcc'btc
--   , path $ usd'bcc :-:   bcc'btc :-: R btc'usd
--   , path $ usd'btc :-:   btc'bcc :-: R bcc'usd
--   , path $ R bcc'usd
--   , path $ bcc'btc :-: R btc'usd
--   , path $ bcc'btc :-:   btc'usd :-: R usd'bcc
--   , path $ bcc'usd :-:   usd'btc :-: R btc'bcc
--   , path $ R btc'bcc
--   , path $ btc'usd :-: R usd'bcc
--   , path $ btc'usd :-:   usd'bcc :-: R bcc'btc
--   , path $ btc'bcc :-:   bcc'usd :-: R usd'btc
--   , path $ R bcc'btc
--   , path $ bcc'usd :-: R usd'btc
--   , path $ bcc'usd :-:   usd'btc :-: R btc'bcc
--   , path $ R btc'usd
--   , path $ btc'bcc :-: R bcc'usd
--   , path $ btc'bcc :-:   bcc'usd :-: R usd'btc
--   --
--   -- , path $ usd'eth :-: R eth'btc
--   -- , path $ R usd'eth
--   -- , path $ usd'btc :-: R btc'eth
--   -- , path $ R btc'eth
--   -- , path $ btc'usd :-: R usd'eth
--   -- , path $ btc'eth :-: R eth'usd
--   -- , path $ R eth'usd
--   -- , path $ eth'btc :-: R btc'usd
--   ]

-- ppp ∷ Path → IO ()
-- ppp (Path route rate) =
--   printf "%20s: %s\n" (intercalate "→" $ (ppACu <$> describe route)) (show (rRate rate))

-- ppr ∷ forall a . Route a → IO ()
-- ppr route = ppp $ path route

-- pp ∷ Rate k '(USDT, BTC) → Rate k '(USDT, BCC) → Rate k '(BTC, BCC) → Rate k '(USDT, ETH) → Rate k '(BTC, ETH) → IO ()
-- pp ub uc bc ue be =
--   mapM_ ppp (paths ub uc ue be bc)
