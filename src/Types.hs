{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Types
where

import           Data.Aeson                   hiding (pairs)
import           Data.Char                           (toLower, toUpper)
import           Data.Foldable                       (asum)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                    as      T
import           GHC.Generics                        (Generic)
import           GHC.Types                           (Symbol, Type)
import           Prelude.Unicode
import           Text.Printf                         (printf)


data Direction
  = Bid
  | Ask
  | X Symbol
  deriving (Show, Generic)

deriving instance Show Symbol

data Dir (a ∷ Direction) where
  BID  ∷ Dir Bid
  ASK  ∷ Dir Ask
  Last ∷ Dir (X "Last")
deriving instance Show (Dir a)


-- * Pretty-printing
--
class PP a where
  pp    ∷ a → Text
  brief ∷ a → Text
  brief = pp


-- * Currency
data Sym
  = USDT | BTC | BCC | ETH | ETC | ZEC | ANS | DASH
  -- | VTC | PPC | FTC | RDD | NXT | DASH | POT | BLK | EMC2 | MYR
  -- | AUR | UTC | MZC | EFL | GLD | FAIR | SLR | PTC | GRS | NLG | RBY | XWC | MONA | BITS
  -- | OC | THC | ENRG | SFR | ERC | NAUT | VRC | CURE | BLC | XC | XDQ | XBB | HYPER | CCN
  -- | XMR | CLOAK | BSD | CRYPT | START | KORE | XDN | TRK | TRUST | NAV | XST | APEX | BTCD
  -- | VIA | TRI | UNO | PINK | IOC | MAX | LXC | BOB | CANN | FC2 | SSD | J | SYS | NEOS | DGB
  -- | ROOT | BTS | BURST | TIT | BSTY | PXI | DGC | SLG | STV | EXCL | SWIFT | NET | GHC | DOPE
  -- | BLOCK | ABY | VIOR | BYC | UFO | XMG | XQN | BLITZ | VPN | BAY | DTC | AM | METAL | SPR
  -- | VTR | XPY | XRP | GAME | GP | NXS | COVAL | FSC2 | SOON | HZ | XCP | BITB | XTC | XVG
  -- | GEO | FLDC | GEMZ | GRC | XCO | MTR | FLO | U | NBT | XEM | MUE | XVC | CLAM | XSEED
  -- | NTRN | SLING | DMD | GAM | UNIT | GRT | VIRAL | SPHR | ARB | OK | ADC | SNRG | PKB
  -- | TES | CPC | AEON | BITZ | ETH | GCR | TX | BCY | PRIME | EXP | NEU | SWING | INFX | OMNI
  -- | USDT | AMP | AGRS | XLM | SPRTS | YBC | BTA | MEC | BITCNY | AMS | SCRT | SCOT | CLUB
  -- | VOX | MND | EMC | FCT | MAID | FRK | EGC | SLS | ORB | STEPS | RADS | DCR | SEC | PIVX
  -- | WARP | CRBIT | MEME | STEEM | LSK | KR | PDC | DGD | BRK | WAVES | RISE | LBC | SBD | BRX
  -- | DRACO | ETC | UNIQ | STRAT | UNB | SYNX | TRIG | EBST | VRM | XAUR | SEQ | SNGLS | REP
  -- | SHIFT | ARDR | XZC | ANS | ZEC | ZCL | IOP | DAR | GOLOS | GBG | UBQ | HKG | KMD | SIB
  -- | ION | LMC | QWARK | CRW | SWT | TIME | MLN | TKS | ARK | DYN | MUSIC | DTB | INCNT
  -- | GBYTE | GNT | NXC | EDG | LGD | TRST | WINGS | RLC | GNO | GUP | LUN | APX | TKN | HMQ
  -- | ANT | ZEN | SC | BAT | QRL | CRB | TROLL | PTOY | MYST | CFI | BNT | NMR | SNT | DCT
  -- | XEL | MCO | ADT | FUN | PAY | MTL | STORJ | ADX | OMG | CVC | PART | QTUM | BCC | DNT
  -- | 8BIT
  -- | 2GIVE
  -- | 1ST
  deriving (Bounded, Enum, Generic, Show, Read)

data Cu (a ∷ Sym) where
  Usdt ∷ Cu USDT
  Btc  ∷ Cu BTC
  Bcc  ∷ Cu BCC
  Eth  ∷ Cu ETH
  Etc  ∷ Cu ETC
  Zec  ∷ Cu ZEC
  Ans  ∷ Cu ANS
  Dash ∷ Cu DASH
deriving instance Show (Cu a)
instance PP (Cu a) where
  pp = upperShowT

cu'sym ∷ Cu a → Sym
cu'sym = read . (toUpper <$>) . show

-- data ACu where
--   ACu ∷ { fromACu ∷ Cu a } → ACu

data Syms where
  Syms ∷
    { s'base   ∷ Sym
    , s'market ∷ Sym
    } → Syms
  deriving (Generic, Show)
instance PP Syms where
  pp (Syms a b) = showT a <> "-" <> showT b

data Pair a b where
  Pair ∷
    { p'base   ∷ Cu a
    , p'market ∷ Cu b
    } → Pair a b
  deriving (Generic, Show)
instance PP (Pair a b) where
  pp (Pair a b) = upperShowT a <> "-" <> upperShowT b

class HasPair (p ∷ Type) where
  type family BidSym p ∷ Sym
  type family AskSym p ∷ Sym
  syms  ∷ p → Syms
  pair  ∷ p → Pair (BidSym p) (AskSym p)
instance HasPair (Pair a b) where
  type BidSym (Pair a b) = a
  type AskSym (Pair a b) = b
  syms (Pair a b) = Syms (cu'sym a) (cu'sym b)
  pair = id

data APair where
  PA ∷ Pair a b → APair
instance PP APair where
  pp (PA p) = pp p


data Rate (k ∷ Direction) t where
  Rate ∷ Dir k → Cu a → Cu b → Double → Rate k '(a, b)
deriving instance Show (Rate k a)
instance HasPair (Rate k '(a, b)) where
  type BidSym (Rate k '(a, b)) = a
  type AskSym (Rate k '(a, b)) = b
  syms  (Rate _ a b _) = Syms (cu'sym a) (cu'sym b)
  pair  (Rate _ a b _) = Pair a b
rate'char ∷ Dir k → Char
rate'char BID    = '>'
rate'char ASK    = '<'
rate'char Last   = '~'
instance PP (Rate k '(a, b)) where
  brief (Rate d _ _ r) = T.pack $ printf     "%c %-5.7f" (rate'char d) r
  pp    (Rate d a b r) = T.pack $ printf "%s%c%s %-5.7f" (show a) (rate'char d) (show b) r

data Market (a ∷ Sym) (b ∷ Sym) where
  Market ∷
    { mk'bid  ∷ Rate Bid '(a, b)
    , mk'ask  ∷ Rate Ask '(a, b)
    , mk'last ∷ Rate (X "Last") '(a, b)
    } → Market a b
deriving instance Show (Market a b)
instance HasPair (Market a b) where
  type BidSym (Market a _) = a
  type AskSym (Market _ b) = b
  pair  = pair ∘ mk'bid
  syms  = syms ∘ mk'bid


-- * Order book
--
data Order (k ∷ Direction) a b where
  Order ∷
    { rate   ∷ Rate k '(a, b)
    , volume ∷ Double
    } → Order k a b
    deriving Show
instance HasPair (Order k a b) where
  type BidSym (Order k a _) = a
  type AskSym (Order k _ b) = b
  pair  = pair ∘ rate
  syms  = syms ∘ rate

data Book a b where
  Book ∷
    { bids ∷ [Order Bid a b]
    , asks ∷ [Order Ask a b]
    } → Book a b
    deriving Show


-- * Trade
--
data Trade where
  Trade ∷
    { market ∷ Market a b
    , book   ∷ Book   a b
    } → Trade


-- * Value computations
--
data Val a where
  Val ∷ Cu a → Double → Val a
deriving instance Show (Val a)


-- * Combinators
--
fromRate ∷ Rate k '(a, b) → Double
inverse  ∷ Rate k '(a, b) → Rate k '(b, a)
-- median   ∷ Rate Bid '(a, b) → Rate Ask '(a, b) → Rate Median '(a, b)

fromRate (Rate _ _ _ r) = r
inverse  (Rate k a b r) = Rate k b a (1 / r)
-- median bid (Rate _ symb syma ask) = Rate MedianR symb syma $ (fromRate bid + ask) * 0.5

-- market'median   ∷ Market a b → Rate Median '(a, b)
-- market'median (Market bid ask _) = median bid ask

-- compose'medians ∷ Rate Median '(a, b) → Rate Median '(b, c) → Rate Median '(a, c)
-- compose'medians  (Rate _ a _ ab) (Rate _ _ c bc) = Rate MedianR a c (ab * bc)


-- * Utils
--
showT ∷ Show a ⇒ a → Text
showT = T.pack ∘ show

lowerShowT ∷ Show a ⇒ a → Text
lowerShowT = T.pack ∘ (toLower <$>) ∘ show

upperShowT ∷ Show a ⇒ a → Text
upperShowT = T.pack ∘ (toUpper <$>) ∘ show

errorT ∷ Text → a
errorT = error ∘ T.unpack

-- * These turn (1 ∷ Int) into (int 1).
int ∷ Int → Int
int = id

str ∷ String → String
str = id


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
instance FromJSON Direction where
  parseJSON = withObject "direction" $
    \_ → asum [ pure Bid, pure Ask ]
instance FromJSON FillType
