{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -Wno-unticked-promoted-constructors -Wno-orphans -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, OverloadedStrings, PartialTypeSignatures, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE DataKinds, DeriveFunctor, DeriveTraversable, EmptyCase, NoMonomorphismRestriction, RankNTypes #-}

module Main (main)
where

import qualified Brick.Main                       as M
-- import qualified Brick.Types                      as T
import qualified Brick.Widgets.Border             as W
-- import qualified Brick.Widgets.Center             as W
import qualified Brick.Widgets.Core               as W
-- import qualified Brick.Widgets.Edit               as W
import qualified Brick.Widgets.List               as W
-- import qualified Brick.AttrMap                    as A
import           Brick.AttrMap                       (AttrMap, AttrName, attrMap)
import           Brick.BChan                         (BChan, newBChan, writeBChan)
-- import           Brick.Widgets.Core
import           Brick.Widgets.List                  (List)
import           Brick.Types                         (Widget, EventM, Next, BrickEvent(..))
import           Brick.Util                          (on, fg)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Control.Monad                       (forM_)
-- import           Control.Lens                        ((<&>))
import           Data.Foldable
-- import           Data.Function                       ((&))
import           Data.List                    hiding (lines)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Text                           (Text)
import           Data.Maybe
import           Data.Monoid                         ((<>))
import           Data.Singletons
import           Data.Traversable
import qualified Data.Vector                      as V
-- import qualified Debug.Trace                      as D
-- import qualified Graphics.Text.Width              as TW
-- import qualified Graphics.Vty                     as V
import           Prelude                      hiding (lines)
import           Prelude.Unicode
import           Safe                                (headMay)
import           Text.Printf                         (printf)


import qualified Graphics.Vty                     as Vty


-- * Local imports
--
import           Types
import qualified Bittrex                          as Bittrex
import           GenericClient
import           Triangle
-- import           Harum


-- * HUD
instance PP (Order k a b) where
  pp Order{..} = T.pack $ printf "%-*s %*.7f" (int 14) (brief rate) (int 11) volume

-- trade'hud ∷ Int → A'Trade → [Text]
-- trade'hud depth (A'Trade (Trade m@Market{..} Book{..})) =
--   let SPair{..}    = pair m
--       info         = T.pack $ printf ". %-*.7f" (int 11) (fromRate mk'last)
--       asks'        = take depth (pp <$> asks)
--       bids'        = take depth (pp <$> bids)
--       width        = fromMaybe 22 $ (T.length <$> headMay asks')
--       head'left    = show p'base
--       head'right   = show p'market
--       header       = T.pack $ printf "%-*s%*s" (width `div` 2 + width `mod` 2) head'left (width `div` 2) head'right
--   in [    header ]
--      <>   reverse asks'
--      <> [ info ]
--      <>           bids'

-- hud'frame ∷ ClientM ()
-- hud'frame = do
--   trades ← sequence [ A'Trade <$> get'trade pa
--                     | A'Pair pa ← default'pairs ]
--   let huds      = trade'hud 10 <$> trades
--       hud'ws    = (maximum ∘ (T.length <$>)) <$> huds
--       hud'lines = transpose huds
--       hud       = [ T.concat ∘ intersperse "  │  "
--                     $ (uncurry justify <$> zip widths lines)
--                   | (widths, lines) ← zip (repeat hud'ws) hud'lines]
--       justify w s = T.pack $ printf "%-*s" w s
--   liftIO $ forM_ hud T.putStrLn


listAttrName, listSelAttrName, activeColAttrName, selActiveColAttrName :: AttrName
listAttrName = W.listAttr <> "custom"
listSelAttrName = W.listSelectedAttr <> "custom-selected"
activeColAttrName = listAttrName <> "active-column"
selActiveColAttrName = listSelAttrName <> "selected-active-column"

attrs ∷ [(AttrName, Vty.Attr)]
attrs = [ (listAttrName, Vty.white `on` Vty.black)
        , (listSelAttrName, Vty.black `on` Vty.yellow)
        , (activeColAttrName, fg Vty.white)
        , (selActiveColAttrName, Vty.black `on` Vty.green)
        , (W.listSelectedFocusedAttr, fg Vty.red)
        ]

attributesMap ∷ AttrMap
attributesMap = attrMap Vty.defAttr $ concat
  [ attrs
  ]

data HEv
  =          TTY               Vty.Event
  | ∀ a b.   A'BookRefresh    (Book a b)
  | ∀ k a b. A'OrderRefresh   (Order k a b) -- XXX: stub
  -- | ∀ a.     BalanceRefresh               -- XXX: stub


default'pairs ∷ [] A'Pair
default'pairs =
  [ A'Pair $ SPair SUSDT SBTC
  , A'Pair $ SPair SUSDT SBCC
  , A'Pair $ SPair SBTC  SBCC
  ]

data UIName = UIName Text deriving (Show, Eq, Ord)

-- WHY: this constructor doesn't package the existentials because it wouldn't allow
--      enforcing consistency, which is otherwise allowed by the binding.
data TradeState a b = TradeState
  { _trade             ∷ Trade a b
  , _listAsks          ∷ List UIName (Order Ask a b)
  , _listBids          ∷ List UIName (Order Bid a b)
  , _orderFilter       ∷ ∀ k. Maybe ([Order k a b] -> [Order k a b])
  }
-- WHY: this existential wrapper is required so that TradeState can bind the type
--      variables for its content.
data A'TradeState = ∀ a b. A'TradeState (TradeState a b)

state'of'trade ∷ Int → Trade a b → TradeState a b
state'of'trade depth trade@(Trade market book) =
  let p'name = pp $ pair market
  in TradeState
     { _trade       = trade
     , _listAsks    = W.list (UIName $ "asks-" <> p'name) (V.fromList ∘ reverse ∘ take depth ∘ asks $ book) 1
     , _listBids    = W.list (UIName $ "bids-" <> p'name) (V.fromList ∘ id      ∘ take depth ∘ bids $ book) 1
     , _orderFilter = Nothing
     }

data Zipper a
  =  Zipper
  { lefts  ∷ [a]
  , this   ∷ a
  , rights ∷ [a]
  } deriving (Functor, Foldable, Traversable)
rootZ ∷ [a] → Zipper a
rootZ []     = error "Asked to create an empty list zipper.  Ain't fun."
rootZ (x:xs) = Zipper [] x xs
count ∷ Num a ⇒ Zipper a → a
count = foldl (\n _->n+1) 0
focus ∷ Zipper a → a
focus (Zipper _ z _) = z
to'left, to'right ∷ Zipper a → Zipper a
to'left x@(Zipper [] _ _) = x
to'left   (Zipper (z':xs) z ys) = Zipper xs z' (z:ys)
to'right x@(Zipper _ _ []) = x
to'right  (Zipper xs z (z':ys)) = Zipper (z:xs) z' ys

rezip ∷ Zipper a → [a]
rezip Zipper{..} = reverse lefts <> [this] <> rights

data TopIx = HelpScreen | TradeScreen
type TradeStates = Zipper A'TradeState
data Status = Connected | Lagging | Disconnected deriving Show
data AppState
  = AppState
  { _screen            ∷ TopIx
  , _tradestates       ∷ TradeStates
  , _status            ∷ Status
  }
initial'state ∷ [] A'Trade → AppState
initial'state []       = error $ printf "No wanna trade?  Why come?"
initial'state trades   = AppState
  { _screen            = TradeScreen
  , _tradestates       = to'right $
                         rootZ [ A'TradeState $ state'of'trade 30 trade
                               | A'Trade trade ← trades ] -- Painting: existential shovel.
  , _status            = Connected
  }

update'book ∷ HEv → AppState → NextState UIName
update'book (A'BookRefresh b) z =
  let _pa = pair b
  in (⊥)


type NextState n = EventM n (Next AppState)

appEvent ∷ AppState → BrickEvent UIName HEv → NextState UIName
appEvent state (AppEvent b@(A'BookRefresh _)) = update'book b state
-- appEvent state (AppEvent (OrderRefresh o)) = update'orders  state
-- appEvent state (AppEvent BalanceRefresh)   = update'balance state
appEvent state@AppState{..} (VtyEvent x)
  | (Vty.EvKey Vty.KLeft [])  ← x = M.continue $ state { _tradestates = to'left  _tradestates }
  | (Vty.EvKey Vty.KRight []) ← x = M.continue $ state { _tradestates = to'right _tradestates }
  | (Vty.EvKey Vty.KEsc  [])  ← x = M.halt state
  | otherwise                     = M.continue state
  --   True → handleViewEvent state vtyEvent
  --   False → case vtyEvent of
  --     VtyEvent (Vty.EvKey (Vty.KChar '?') []) → M.continue $ state & helpActive %~ not
  --     VtyEvent (Vty.EvKey (Vty.KChar 'p') []) → void (liftIO togglePlayPause) >> M.continue state
  --     VtyEvent (Vty.EvKey (Vty.KChar 'c') []) → void (liftIO clearPlaylist) >> M.continue state
  --     VtyEvent (Vty.EvKey (Vty.KChar '1') []) → M.continue $ state & activeView .~ PlaylistView
  --     VtyEvent (Vty.EvKey (Vty.KChar '2') []) → M.continue $ state & activeView .~ LibraryView
  --     VtyEvent (Vty.EvKey (Vty.KChar '-') []) → void (liftIO (decreaseVolume state)) >> M.continue state
  --     VtyEvent (Vty.EvKey (Vty.KChar '+') []) → void (liftIO (increaseVolume state)) >> M.continue state
  --     ev → handleViewEvent state ev

draw'help ∷ [Widget n]
draw'help = (⊥)

draw'tradestate ∷ Bool → A'TradeState → Widget UIName
draw'tradestate focused (A'TradeState TradeState{..}) = W.vBox
  $  [W.txt (pp $ pair _trade)]
  <> [render'list _listAsks]
  <> [W.txt "-----------------"]
  <> [render'list _listBids]
  where
    render'list ∷ ∀ a b c. List UIName (Order c a b) → Widget UIName
    render'list xs = W.renderList (\_sel x → W.txt $ pp x) focused xs

draw'tradestates ∷ TradeStates → [Widget UIName]
draw'tradestates (Zipper lefts this rights) =  (:[]) ∘ W.hBox
  $  intersperse W.vBorder
  $  (draw'tradestate False <$> reverse lefts)
  <> [draw'tradestate True this]
  <> (draw'tradestate False <$> id     rights)

drawUI ∷ AppState → [Widget UIName]
drawUI AppState{..} =
  case _screen of
    HelpScreen  → draw'help
    TradeScreen → draw'tradestates _tradestates

app ∷ M.App AppState HEv UIName
app = M.App
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent  = appEvent
  , M.appStartEvent   = pure
  , M.appAttrMap      = const attributesMap
  }

watch'triangle ∷ (SingI a, SingI b, SingI c) ⇒ SPair' a b → SPair' b c → SPair' a c → Val c → IO ()
watch'triangle pab pbc pac value = Bittrex.runHTTP loop
  where
    loop = do
      m'a'b ← get'market pab
      m'a'c ← get'market pac
      m'b'c ← get'market pbc
      let cost = TxnCostFactor 1.0025
          -- x ∷ (TriFactor a b c, TriFactor a c b, Val a, Val a)
          x@(triabc, triacb, vabc, vacb) =  triangulate cost value m'a'b m'b'c m'a'c
      liftIO $ printf "%s → %s → %s: %f / %f | %s → %s → %s: %f / %f"
        (pp a) (pp b) (pp c)
        (fromTriFactor triabc) (v'val vabc)
        (fromTriFactor triacb) (v'val vacb)
      loop

watch'triangle'SBCC'SBTC'SUSDT ∷ Val BCC → IO ()
watch'triangle'SBCC'SBTC'SUSDT =
  watch'triangle (Pair SBTC SBCC) (Pair SUSDT SBCC) (Pair SUSDT SBTC)

main ∷ IO ()
main = -- Bittrex.runHTTP $ do
  -- event'chan ← liftIO $ newBChan 10
  -- trades ← sequence [ A'Trade <$> get'trade p
  --                   | A'Pair p ← default'pairs ]
  -- liftIO $ M.customMain (Vty.mkVty Vty.defaultConfig)
  --   (Just event'chan) app (initial'state trades)
  -- pure ()

  watch'triangle SBCC SBTC SUSDT (undefined)

-- *
-- → https://bittrex.com/Market/Index?MarketName=BTC-BCC
-- ← Set-Cookie:__cfduid=d2bcf3255b9cd2e981bed73dc7d452b611502627404
--    <link href="/Content/vendor/cryptocoins/cryptocoins.css" rel="stylesheet" />
--    <link href="/content/tpcore?v=QJ3wNu5EQ6UlCjsTJdbaOLSWB_y7J7AB8BJJdZDroBo1" rel="stylesheet"/>
--    <link href="/content/datatables?v=V8Z9lEDy7UIqnRXRLbTNYwvRqsSCW24JRZPrC0xpwOw1" rel="stylesheet"/>
--    <link href="/content/bcore?v=Mu4k6pOKqoJbnH1qGHzl2Wzi9spro5O2h14vkAd5aKo1" rel="stylesheet"/>
--    <script src="/bundles/modernizr?v=inCVuEFe6J4Q07A0AcRsbJic_UE5MwpRMNGcOtk94TE1"></script>
--    <link href="/content/layout?v=c6hTevJcFMkORDVQ88_dTxsP2m39Wu8rHyeychn4N-U1" rel="stylesheet"/>
--    <link href="/content/market?v=IopVw-D8NVFHDhvl-sjFse8c1bQo0BT1WUvm1pfi6Zg1" rel="stylesheet"/>
-- → https://bittrex.com/content/tpcore?v=QJ3wNu5EQ6UlCjsTJdbaOLSWB_y7J7AB8BJJdZDroBo1
-- * 
-- https://socket.bittrex.com/signalr/negotiate?clientProtocol=1.5&connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D&_=1502626465643
-- - signalr/negotiate
-- - clientProtocol=1.5
-- - connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D
-- - _=1502626465643
-- ← {"Url":"/signalr"
--   ,"ConnectionToken":"C9UHW7gWnm4J32xpZTM2py1QT0LG449qhj0PQkRTt3/UlvPgx3s1z0NpgkqQT0uIPcPvZOF/lNpFLDtDPdoK5WMQuPcsEkDewzPBpfeDeRaAGITo"
--   ,"ConnectionId":"9e62c072-f51b-41ed-a927-a556db9d35a5"
--   ,"KeepAliveTimeout":20.0, "DisconnectTimeout":30.0, "ConnectionTimeout":110.0,"TransportConnectTimeout":5.0,"LongPollDelay":0.0
--   ,"TryWebSockets":true,"ProtocolVersion":"1.5"}
-- ← {"Url":"/signalr"
--   ,"ConnectionToken":"9Mdp5sUm0Z6+f9a+X1SnH8K1okN1GT3H4/s0FfH31bM7f1iVpWT9xUsnWh3UBFO+R9FPxKy4bbPVA7Gkbeb5L+sIZGE82ngEVITlHv8dncFwTRFs"
--   ,"ConnectionId":"614a6f3f-6fc4-49b1-b6a7-02fac8e688dc"
--   ,"KeepAliveTimeout":20.0, "DisconnectTimeout":30.0, "ConnectionTimeout":110.0,"TransportConnectTimeout":5.0,"LongPollDelay":0.0
--   ,"TryWebSockets":true,"ProtocolVersion":"1.5"}
-- https://socket.bittrex.com/signalr/start?transport=webSockets&clientProtocol=1.5&connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j&connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D&_=1502626465644
-- - signalr/start
-- - transport=webSockets
-- - clientProtocol=1.5
-- - connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j
-- - connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D
-- - _=1502626465644
-- wss://socket.bittrex.com/signalr/connect?transport=webSockets&clientProtocol=1.5&connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j&connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D&tid=6
-- - signalr/connect
-- - transport=webSockets
-- - clientProtocol=1.5
-- - connectionToken=9sFtmdh6rzbX3ZP4%2F%2B7zPgCa5OTrgL9AKq4loyo03fvG5ZWoGb3bFJe3XxXYqhZYXzHdKEPlG%2B1ij4Im1HDlYyH%2BpsjmSTzFk6e%2FQPY%2BiBQ3z36j
-- - connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D
-- - tid=6
--
-- *
-- /Api/v2.0/auth/market/GetOrderHistory
-- /Api/v2.0/pub/currencies/GetBTCPrice
-- /api/v2.0/pub/Markets/GetMarketSummaries
