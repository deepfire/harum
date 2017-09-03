{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, PartialTypeSignatures, RankNTypes, RecordWildCards, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Triangle
where

import           Data.Singletons
import           GHC.Types                           (Symbol, Type)
import           GHC.TypeLits
import           Prelude.Unicode

import           Types


-- * Type-levels for Pair/Triangle
--
type family ShowTri (ab ∷ Pair) (bc ∷ Pair) (ac ∷ Pair) ∷ ErrorMessage where
  ShowTri ('Pair sa sb) ('Pair sb sc) ('Pair sa sc) = Text "(" :<>: ShowPair ('Pair sa sb) :<>: Text " " :<>: ShowPair ('Pair sb sc) :<>: Text " " :<>: ShowPair ('Pair sa sc) :<>: Text ")"
  ShowTri ab bc ac = Text "#<Non-triangle " :<>: ShowPair ab :<>: Text " " :<>: ShowPair bc :<>: Text " " :<>: ShowPair ac :<>: Text ">"

type family S (s ∷ Sym) (d ∷ Dir) (p ∷ Pair) where
  S _  act ('Pair sa sa) = TypeError (Text "Idempotent step from " :<>: ShowType sa :<>: Text ", action " :<>: ShowType act)
  S sa Bid ('Pair sa sb) = '(sa, Bid, ('Pair sa sb))
  S sb Ask ('Pair sa sb) = '(sb, Ask, ('Pair sa sb))
  S sc act ('Pair sa sb) = TypeError (Text "Invalid step from "    :<>: ShowType sc :<>: Text ", in " :<>: ShowPair ('Pair sa sb) :<>: Text ", action " :<>: ShowType act)

type family TriangleRoutes (a ∷ Pair) (b ∷ Pair) (c ∷ Pair) (from ∷ Sym) ∷ (Sym, [(Sym, Dir, Pair)], [(Sym, Dir, Pair)]) where
  TriangleRoutes ('Pair x x) _           _           _ = TypeError (Text "Triangle's first pair is idempotent: "  :<>: ShowPair ('Pair x x))
  TriangleRoutes _           ('Pair x x) _           _ = TypeError (Text "Triangle's second pair is idempotent: " :<>: ShowPair ('Pair x x))
  TriangleRoutes _           _           ('Pair x x) _ = TypeError (Text "Triangle's third pair is idempotent: "  :<>: ShowPair ('Pair x x))
  TriangleRoutes ('Pair a b) ('Pair b c) ('Pair a c) a = '(,,) a '[S a Bid ('Pair a b), S b Bid ('Pair b c), S c Ask ('Pair a c)]
                                                                 '[S a Bid ('Pair a c), S c Ask ('Pair b c), S b Ask ('Pair a b)]
  TriangleRoutes ('Pair a b) ('Pair b c) ('Pair a c) b = '(,,) b '[S b Bid ('Pair b c), S c Ask ('Pair a c), S a Bid ('Pair a b)]
                                                                 '[S b Ask ('Pair a b), S a Bid ('Pair a c), S c Ask ('Pair b c)]
  TriangleRoutes ('Pair a b) ('Pair b c) ('Pair a c) c = '(,,) c '[S c Ask ('Pair a c), S a Bid ('Pair a b), S b Bid ('Pair b c)]
                                                                 '[S c Ask ('Pair b c), S b Ask ('Pair a b), S a Bid ('Pair a c)]
  TriangleRoutes ('Pair a b) ('Pair b c) ('Pair a c) x =
    TypeError (Text "Source " :<>: ShowType x :<>: Text " doesn't belong to triangle " :<>: ShowTri ('Pair a b) ('Pair b c) ('Pair a c))
  TriangleRoutes _       _       _       _             = TypeError (Text "Not a proper AB-BC-AC triangle.")

type TriangleRts a b c d = TriangleRoutes ('Pair a b) ('Pair b c) ('Pair a c) d

type TriRouteLeft  ab bc ac d = Snd3 (TriangleRoutes ab bc ac d)
type TriRouteRight ab bc ac d = Trd3 (TriangleRoutes ab bc ac d)

type TriRtLeft     a  b   c d = Snd3 (TriangleRts    a  b   c d)
type TriRtRight    a  b   c d = Trd3 (TriangleRts    a  b   c d)

class RouteProducer (xs ∷ [ActSpec]) where
  produce'route ∷ Action xs

instance (SingI dir, SingI pair, SingI from, RouteProducer remainder) ⇒ RouteProducer ('(from, dir, pair) : remainder) where
  produce'route = Action sing sing sing produce'route
instance RouteProducer '[] where
  produce'route = Done

triangle'actions ∷ (RouteProducer (TriRtLeft  a b c  d),
                    RouteProducer (TriRtRight a b c  d))
                 ⇒                 SPair'     a b  
                 →                 SPair'       b c
                 →                 SPair'     a   c
                 →                 SSym              d
                 → (Action        (TriRtLeft  a b c  d),
                    Action        (TriRtRight a b c  d))
triangle'actions _ _ _ _ = (,) produce'route produce'route

-- type GoodTriangle a = TriangleRoutes ('Pair USDT BTC) ('Pair BTC BCC) ('Pair USDT BCC) a
-- type  BadTriangle   = TriangleRoutes ('Pair BTC  BCC) ('Pair BCC BTC) ('Pair BTC  BCC) USDT

-- triangle'routes (SPair SUSDT SBTC) (SPair SBTC SBCC) (SPair SUSDT SBCC) SUSDT
-- triangle'routes (SPair SUSDT SBTC) (SPair SBTC SBCC) (SPair SUSDT SBCC) SUSDT
--   :: (Action
--         '['('Bid, 'Pair 'USDT 'BTC, 'USDT), '('Bid, 'Pair 'BTC 'BCC, 'BTC),
--           '('Ask, 'Pair 'USDT 'BCC, 'BCC)],
--       Action
--         '['('Bid, 'Pair 'USDT 'BCC, 'USDT), '('Ask, 'Pair 'BTC 'BCC, 'BCC),
--           '('Ask, 'Pair 'USDT 'BTC, 'BTC)])




newtype TriFactor a b c = TriFactor { fromTriFactor ∷ Double } deriving (Eq, Ord, Show)

tri'mult ∷ TriFactor a b c → Val a → Val a
tri'mult f v = Val (v'sym v) (v'val v ⋅ fromTriFactor f)

-- We have.. ?
triangulate ∷ TxnCostFactor → Market a b → Market b c → Market a c → Val a → (TriFactor a b c, TriFactor a c b, Val a, Val a)
triangulate cost (Market bid'a'b ask'a'b _) (Market bid'b'c ask'b'c _) (Market bid'a'c ask'a'c _) Val{..} =
  let (,) rtl rtr = triangle'actions sing sing sing v'sym
      trif'abca  = route'abca cost value bid'a'b bid'b'c ask'a'c
      trif'acba  = route'acba cost value bid'a'c ask'b'c ask'a'b
  in (,,,)           trif'abca                  trif'acba
           (tri'mult trif'abca value) (tri'mult trif'acba value)

-- type family PairOptimalPrice (a ∷ Sym) (b ∷ Sym) (from ∷ Sym) ∷ Dir where
--   PairOptimalPrice sx sx sy = TypeError (Text "Invalid pair: " :<>: ShowPair '(sx, sx))
--   PairOptimalPrice sa sb sa = Bid
--   PairOptimalPrice sa sb sb = Ask
--   PairOptimalPrice sa sb sx = TypeError (Text "Unroutable source " :<>: ShowType sx :<>: Text " for pair " :<>: ShowPair '(sa, sb))

-- mk'optim'rate ∷ Market '(a, b) → Act (PairOptimalPrice a b from) → Val from → Rate (PairOptimalPrice a b from) '(a, b)
-- mk'optim'rate   Market{..}   Buy                               _        = mk'bid 
-- mk'optim'rate   Market{..}   Sell                              _        = mk'ask 

-- -- mk'optim'step ∷ Market '(a, b) → Act (PairOptimalPrice a b from) → Val from → Market
-- -- mk'optim'rate ∷ Val from → Act d → Market '(a, b) → Rate (PairOptimalPrice a b from) '(a, b)
-- -- mk'optim'rate   _          Buy     Market{..} = mk'bid 
-- -- mk'optim'rate   _          Sell    Market{..} = mk'ask 

-- trade'optimistic ∷ Act d → Rate d '(a,   b)      → Val (PairFrom d a b) → Val (PairTo d a b)
-- trade'optimistic   Buy    (Rate SBid _    to rate) (Val _ value)         = Val to   (value / rate)
-- trade'optimistic   Sell   (Rate SAsk from _  rate) (Val _ value)         = Val from (value * rate)

-- buy'bid  ∷ Val a  → Rate Bid '(a, b) → (,,) (Act Bid) (SDir Bid) (Val b)
-- buy'bid    Val{..} (Rate _ _ _ f)    = (,,)  Buy       SBid      (Val sing (v'val / f))
-- sell'ask ∷ Val b  → Rate Ask '(a, b) → (,,) (Act Ask) (SDir Ask) (Val a)
-- sell'ask   Val{..} (Rate _ _ _ f)    = (,,)  Sell      SAsk      (Val sing (v'val * f))
