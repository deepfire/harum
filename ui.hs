{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, PartialTypeSignatures, RecordWildCards, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE DataKinds, DeriveFunctor, DeriveTraversable, EmptyCase, NoMonomorphismRestriction, RankNTypes #-}

module Main (main)
where

import qualified Brick.Main                       as M
import qualified Brick.Types                      as T
import qualified Brick.Widgets.Border             as W
import qualified Brick.Widgets.Center             as W
import qualified Brick.Widgets.Core               as W
import qualified Brick.Widgets.Edit               as W
import qualified Brick.Widgets.List               as W
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
import           Control.Lens                        ((<&>))
import           Data.Foldable
import           Data.Function                       ((&))
import           Data.List                    hiding (lines)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Text                           (Text)
import           Data.Maybe
import           Data.Monoid                         ((<>))
import           Data.Traversable
import qualified Data.Vector                      as V
import qualified Debug.Trace                      as D
import qualified Graphics.Text.Width              as TW
import qualified Graphics.Vty                     as V
import           Prelude                      hiding (lines)
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
instance PP (Order k a b) where
  pp Order{..} = T.pack $ printf "%-*s %*.7f" (int 14) (brief rate) (int 11) volume

trade'hud ∷ Int → A'Trade → [Text]
trade'hud depth (A'Trade (Trade m@Market{..} Book{..})) =
  let Pair{..}     = pair m
      info         = T.pack $ printf ". %-*.7f" (int 11) (fromRate mk'last)
      asks'        = take depth (pp <$> asks)
      bids'        = take depth (pp <$> bids)
      width        = fromMaybe 22 $ (T.length <$> headMay asks')
      head'left    = show p'base
      head'right   = show p'market
      header       = T.pack $ printf "%-*s%*s" (width `div` 2 + width `mod` 2) head'left (width `div` 2) head'right
  in [    header ]
     <>   reverse asks'
     <> [ info ]
     <>           bids'

hud'frame ∷ ClientM ()
hud'frame = do
  trades ← sequence [ A'Trade <$> get'trade pa
                    | A'Pair pa ← default'pairs ]
  let huds      = trade'hud 10 <$> trades
      hud'ws    = (maximum ∘ (T.length <$>)) <$> huds
      hud'lines = transpose huds
      hud       = [ T.concat ∘ intersperse "  │  "
                    $ (uncurry justify <$> zip widths lines)
                  | (widths, lines) ← zip (repeat hud'ws) hud'lines]
      justify w s = T.pack $ printf "%-*s" w s
  liftIO $ forM_ hud T.putStrLn


listAttrName :: AttrName
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
  [ A'Pair $ Pair SUSDT SBTC
  , A'Pair $ Pair SUSDT SBCC
  , A'Pair $ Pair SBTC  SBCC
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
  in TradeState
     { _trade       = trade
     , _listAsks    = W.list (UIName $ "asks-" <> p'name) (V.fromList ∘ reverse ∘ take 15 ∘ asks $ book) 1
     , _listBids    = W.list (UIName $ "bids-" <> p'name) (V.fromList ∘ id      ∘ take 15 ∘ bids $ book) 1
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
rezip Zipper{..} = lefts <> [this] <> rights

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
                         rootZ [ A'TradeState $ state'of'trade trade
                               | A'Trade trade ← trades ] -- Painting: existential shovel.
  , _status            = Connected
  }

update'book ∷ HEv → AppState → NextState UIName
update'book (A'BookRefresh b) z =
  let pa = pair b
  in (⊥)


type NextState n = EventM n (Next AppState)

appEvent ∷ AppState → BrickEvent UIName HEv → NextState UIName
appEvent state (AppEvent b@(A'BookRefresh _)) = update'book b state
-- appEvent state (AppEvent (OrderRefresh o)) = update'orders  state
-- appEvent state (AppEvent BalanceRefresh)   = update'balance state
appEvent state vtyEvent = (⊥)
  -- case state of
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
draw'tradestates xs@(Zipper lefts this rights) =  (:[]) ∘ W.hBox
  $  intersperse W.vBorder
  $  (draw'tradestate False <$> lefts)
  <> [draw'tradestate True this]
  <> (draw'tradestate False <$> rights)

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

main ∷ IO ()
main = Bittrex.run $ do
  event'chan ← liftIO $ newBChan 10
  trades ← sequence [ A'Trade <$> get'trade p
                    | A'Pair p ← default'pairs ]
  liftIO $ M.customMain (Vty.mkVty Vty.defaultConfig)
    (Just event'chan) app (initial'state trades)
  pure ()
