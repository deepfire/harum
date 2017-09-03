{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans -Wno-partial-type-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Harum
where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                    hiding (lines)
import           Data.Maybe
import           Data.Monoid                  hiding (Last)
import           Data.Text                           (Text)
import qualified Data.Text                    as      T
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


-- * A stab at analytics

-- analyse'triangle ∷ Market a b → Market b c → Rate Median '(a, c)
-- analyse'triangle left right =
--   market'median left `compose'medians` market'median right

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
