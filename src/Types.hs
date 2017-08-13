{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, PartialTypeSignatures, RankNTypes, RecordWildCards, StandaloneDeriving, TypeOperators, TypeInType, UnicodeSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types
where

import           Data.Aeson                   hiding (pairs)
import           Data.Char                           (toUpper, toLower)
import           Data.Foldable                       (asum)
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                        as T
import           GHC.Generics                        (Generic)
import           GHC.Stack
import           GHC.Types                           (Symbol, Type)
import           Prelude.Unicode
import           Data.Singletons
import           Data.Singletons.TH
import qualified System.Logger                    as Log
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

$(genSingletons [''Sym])

deriving instance Show (SSym a)
instance PP (SSym a) where
  pp = T.pack ∘ drop 1 ∘ upperShow

data Pair a b where
  Pair ∷
    { p'base   ∷ SSym a
    , p'market ∷ SSym b
    } → Pair a b
  deriving (Generic, Show)
instance PP (Pair a b) where
  pp (Pair a b) = pp a <> "-" <> pp b

class HasPair (p ∷ Type) where
  type family BidSym p ∷ Sym
  type family AskSym p ∷ Sym
  pair  ∷ p → Pair (BidSym p) (AskSym p)
instance HasPair (Pair a b) where
  type BidSym (Pair a b) = a
  type AskSym (Pair a b) = b
  pair = id

data A'Pair where
  A'Pair ∷ (SingI a, SingI b) ⇒ Pair a b → A'Pair
instance PP A'Pair where
  pp (A'Pair p) = pp p


data Rate (k ∷ Direction) t where
  Rate ∷ (SingI a, SingI b) ⇒ Dir k → SSym a → SSym b → Double → Rate k '(a, b)
deriving instance Show (Rate k a)
instance HasPair (Rate k '(a, b)) where
  type BidSym (Rate k '(a, b)) = a
  type AskSym (Rate k '(a, b)) = b
  pair  (Rate _ a b _) = Pair a b
rate'char ∷ Dir k → Char
rate'char BID    = '>'
rate'char ASK    = '<'
rate'char Last   = '~'
instance PP (Rate k '(a, b)) where
  brief (Rate d _ _ r) = T.pack $ printf     "%c %-5.7f" (rate'char d) r
  pp    (Rate d a b r) = T.pack $ printf "%s%c%s %-5.7f" (show a) (rate'char d) (show b) r

data Market (a ∷ Sym) (b ∷ Sym) where
  Market ∷ (SingI a, SingI b) ⇒
    { mk'bid  ∷ Rate Bid '(a, b)
    , mk'ask  ∷ Rate Ask '(a, b)
    , mk'last ∷ Rate (X "Last") '(a, b)
    } → Market a b
deriving instance Show (Market a b)
instance HasPair (Market a b) where
  type BidSym (Market a b) = a
  type AskSym (Market a b) = b
  pair  = pair ∘ mk'bid


-- * Order book
--
data Order (k ∷ Direction) a b where
  Order ∷ (SingI a, SingI b) ⇒
    { rate   ∷ Rate k '(a, b)
    , volume ∷ Double
    } → Order k a b
deriving instance Show (Order k a b)
instance HasPair (Order k a b) where
  type BidSym (Order k a b) = a
  type AskSym (Order k a b) = b
  pair  = pair ∘ rate

data Book (a ∷ Sym) (b ∷ Sym) where
  Book ∷ (SingI a, SingI b) ⇒
    { bids ∷ [Order Bid a b]
    , asks ∷ [Order Ask a b]
    } → Book a b
deriving instance Show (Book a b)
instance HasPair (Book (a ∷ Sym) (b ∷ Sym)) where
  type BidSym (Book a b) = a
  type AskSym (Book a b) = b
  pair (Book _ _)  = Pair sing sing


-- * Trade
--
-- WHY: this doesn't package a and b as existentials, because some upstreams can
--      benefit from more precise types.
data Trade a b where
  Trade ∷
    { market ∷ Market a b
    , book   ∷ Book   a b
    } → Trade a b
data A'Trade = ∀ a b. (SingI a, SingI b) ⇒ A'Trade (Trade a b)
instance HasPair (Trade a b) where
  type BidSym (Trade a b) = a
  type AskSym (Trade a b) = b
  pair Trade{..} = pair market


-- * Value computations
--
data Val a where
  Val ∷ SSym a → Double → Val a
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


-- * System parametrisation
--
data Options where
  Options ∷
    { o'logging ∷ Log.Settings
    } → Options


-- * Utils
--
lowerShow,  upperShow ∷ Show a ⇒ a → String
lowerShow  = (toLower <$>) ∘ show
upperShow  = (toUpper <$>) ∘ show

lowerShowT, upperShowT, showT ∷ Show a ⇒ a → Text
lowerShowT = T.pack ∘ lowerShow
upperShowT = T.pack ∘ upperShow
showT      = T.pack ∘ show

errorT ∷ HasCallStack ⇒ Text → a
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
