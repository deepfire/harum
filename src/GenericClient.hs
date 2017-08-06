{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
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

module GenericClient
  ( module Servant.Client
    --
  , handlerr
  , get'book, get'market, get'trade
  )
where

import           Control.Monad
import           Data.Monoid                  hiding (Last)
import           Prelude.Unicode
import           Servant.Client
import           Text.Printf


-- * Local imports
--
import           Types
import           Bittrex

handlerr ∷ ClientM (Response a) → ClientM a
handlerr action = flt <$> action
  where flt (Response False msg _) = errorT $ "Bittrex error: " <> msg
        flt (Response True  _   x) = x

get'book ∷ Market a b → ClientM (Book a b)
get'book market = do
  DescOrderBook{..} ← handlerr $ getorderbook (Just $ syms market) (Just OBBoth)
  let Pair sbid sask = pair market
      bids = [ Order { rate   = Rate BID sbid sask poRate
                     , volume = poQuantity }
             | DescPosition{..} ← obbuy ]
      asks = [ Order { rate   = Rate ASK sbid sask poRate
                     , volume = poQuantity }
             | DescPosition{..} ← obsell ]
  pure Book{..}

get'market ∷ Pair a b → ClientM (Market a b)
get'market pa@(Pair base mkt) = do
  let ss@Syms{..} = syms pa
      name'wanted = pp   pa
  [DescMarketSummary{..}] ← handlerr $ getmarketsummary $ Just ss
  unless (name'wanted ≡ msMarketName) $
    error $ printf "BTX inconsistency: expected %s, got %s" name'wanted msMarketName
  let mk'bid  = Rate BID  base mkt msBid
      mk'ask  = Rate ASK  base mkt msAsk
      mk'last = Rate Last base mkt msLast
  pure Market{..}

get'trade ∷ Pair a b → ClientM (Trade a b)
get'trade pa = do
  market ← get'market pa
  book   ← get'book market
  pure Trade{..}
