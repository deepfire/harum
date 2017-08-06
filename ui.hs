{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DataKinds, EmptyCase, RankNTypes #-}

module Main (main)
where

import qualified Brick.Main                       as M
import qualified Brick.Types                      as T
import qualified Brick.Widgets.List               as L
import qualified Brick.Widgets.Center             as C
import qualified Brick.Widgets.Edit               as E
import qualified Brick.AttrMap                    as A
import           Brick.AttrMap                       (AttrMap, AttrName, attrMap)
import           Brick.BChan                         (BChan, newBChan, writeBChan)
import           Brick.Widgets.Core
import           Brick.Widgets.List                  (List)
import           Brick.Types                         (Widget, EventM, Next, BrickEvent(..))
import           Brick.Util                          (on, fg, bg)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Control.Monad                       (forM_, void)
import           Control.Zipper
import qualified Control.Zipper.Internal          as Z
import           Control.Lens                        ((<&>))
import           Data.List                    hiding (lines)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Text                           (Text)
import           Data.Maybe
import           Data.Monoid                         ((<>))
import qualified Data.Vector                      as V
import qualified Graphics.Text.Width              as TW
import qualified Graphics.Vty                     as V
import           Prelude.Unicode
import           Safe                                (headMay)
import           System.Environment                  (getArgs)
import           Text.Printf                         (printf)


import qualified Graphics.Vty                     as Vty


-- * Local imports
--
import           Types
import           GenericClient
import           Harum
import qualified Bittrex                          as Bittrex


-- * HUD
trade'hud ∷ Int → A'Trade → [Text]
trade'hud depth (A'Trade (Trade m@Market{..} Book{..})) =
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

hud'frame ∷ ClientM ()
hud'frame = do
  trades ← sequence [ A'Trade <$> get'trade pa
                    | PA pa ← default'pairs ]
  let huds      = trade'hud 10 <$> trades
      hud'ws    = (maximum ∘ (T.length <$>)) <$> huds
      hud'lines = transpose huds
      hud       = [ T.concat ∘ intersperse "  │  "
                    $ (uncurry justify <$> zip widths lines)
                  | (widths, lines) ← zip (repeat hud'ws) hud'lines]
      justify w s = T.pack $ printf "%-*s" w s
  liftIO $ forM_ hud T.putStrLn


listAttrName :: AttrName
listAttrName = L.listAttr <> "custom"
listSelAttrName = L.listSelectedAttr <> "custom-selected"
activeColAttrName = listAttrName <> "active-column"
selActiveColAttrName = listSelAttrName <> "selected-active-column"

attrs ∷ [(AttrName, Vty.Attr)]
attrs = [ (listAttrName, Vty.white `on` Vty.black)
        , (listSelAttrName, Vty.black `on` Vty.yellow)
        , (activeColAttrName, fg Vty.white)
        , (selActiveColAttrName, Vty.black `on` Vty.green)
        , (L.listSelectedFocusedAttr, fg Vty.red)
        ]

attributesMap ∷ AttrMap
attributesMap = attrMap Vty.defAttr $ concat
  [ attrs
  ]

data HEv
  =          TTY             Vty.Event
  | ∀ a b.   BookRefresh    (Book a b)
  | ∀ k a b. OrderRefresh   (Order k a b) -- XXX: stub
  | ∀ a.     BalanceRefresh               -- XXX: stub


default'pairs ∷ [] APair
default'pairs =
  [ PA $ Pair Usdt Btc
  , PA $ Pair Usdt Bcc
  , PA $ Pair Btc  Bcc
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

state'of'trade ∷ Trade a b → TradeState a b
state'of'trade trade@(Trade market book) =
  let p'name = pp $ pair market
      p'bids = bids $ book
  in TradeState
     { _trade       = trade
     , _listAsks    = L.list (UIName $ "asks-" <> p'name) (V.fromList $ asks $ book) 1
     , _listBids    = L.list (UIName $ "bids-" <> p'name) (V.fromList $ bids $ book) 1
     , _orderFilter = Nothing
     }

data Status
  = Connected
  | Lagging
  | Disconnected
  deriving Show

data TopIx = Help | Trades
data AppState
  = AppState
  { _state             ∷ Zipper Top TopIx ([] A'TradeState)
  , _status            ∷ Status
  }

initial'state ∷ [] A'Trade → AppState
initial'state trades   = AppState
  { _state             = Z.zipper' Trades
                         [ A'TradeState $ state'of'trade trade
                           -- Painting: existential shovel.
                         | A'Trade trade ← trades ]
  , _status            = Connected
  }


type NextState n = EventM n (Next AppState)

appEvent ∷ AppState → BrickEvent UIName HEv → NextState UIName
appEvent state (AppEvent (BookRefresh  b)) = update'book state b
-- appEvent state (AppEvent (OrderRefresh o)) = update'orders  state
-- appEvent state (AppEvent BalanceRefresh)   = update'balance state
appEvent state vtyEvent =
  case state^.filterStateL.isFocusedL of
    True → handleViewEvent state vtyEvent
    False → case vtyEvent of
      VtyEvent (Vty.EvKey (Vty.KChar '?') []) → M.continue $ state & helpActive %~ not
      VtyEvent (Vty.EvKey (Vty.KChar 'p') []) → void (liftIO togglePlayPause) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar 'c') []) → void (liftIO clearPlaylist) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar '1') []) → M.continue $ state & activeView .~ PlaylistView
      VtyEvent (Vty.EvKey (Vty.KChar '2') []) → M.continue $ state & activeView .~ LibraryView
      VtyEvent (Vty.EvKey (Vty.KChar '-') []) → void (liftIO (decreaseVolume state)) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar '+') []) → void (liftIO (increaseVolume state)) >> M.continue state
      ev → handleViewEvent state ev

draw'help ∷ [Widget n]
draw'help = (⊥)

drawUI ∷ AppState → [Widget n]
drawUI AppState{..} =
  case Z.focalPoint _state of
    Help   → draw'help
    Trades → draw'trades

app ∷ M.App AppState HEv UIName
app = M.App
  { M.appDraw         = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent  = appEvent
  , M.appStartEvent   = pure
  , M.appAttrMap      = const attributesMap
  }

main ∷ IO ()
main = Bittrex.run $ do
  event'chan ← liftIO $ newBChan 10
  trades ← mapM ((A'Trade <$>) ∘ get'trade) []
  liftIO $ M.customMain (Vty.mkVty Vty.defaultConfig)
    (Just event'chan) app (initial'state trades)
