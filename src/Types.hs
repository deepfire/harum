{-# OPTIONS_GHC -Wall -Wno-unticked-promoted-constructors -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TypeOperators, UnicodeSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Types
where

import           Data.Aeson                   hiding (pairs)
import           Data.Char                           (toUpper, toLower)
import           Data.Foldable                       (asum)
import           Data.Monoid                         ((<>))
import qualified Data.Text                        as T
import           Data.Dependent.Map                  (DMap, DSum)
import qualified Data.Dependent.Map               as D
import qualified Data.Dependent.Sum               as D
import qualified Data.GADT.Compare                as D
import           Data.GADT.Compare
import           Data.GADT.Compare.TH
import           Data.GADT.Show
import           Data.GADT.Show.TH
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Maybe
import           GHC.Generics                        (Generic)
import           GHC.Stack
import           GHC.Types                           (Symbol, Type)
import           GHC.TypeLits
import           Prelude.Unicode
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import qualified System.Logger                    as Log
import           Text.Printf                         (printf)
import qualified Unsafe.Coerce                    as U


-- * Generic type-level (…that likely exist elsewhere…)
--
type family Fst3 (k ∷ (b, c, d))    ∷ b where Fst3 '(x, _, _) = x
type family Snd3 (k ∷ (b, c, d))    ∷ c where Snd3 '(_, x, _) = x
type family Trd3 (k ∷ (b, c, d))    ∷ d where Trd3 '(_, _, x) = x
type family Fst4 (k ∷ (b, c, d, e)) ∷ b where Fst4 '(x, _, _, _) = x
type family Snd4 (k ∷ (b, c, d, e)) ∷ c where Snd4 '(_, x, _, _) = x
type family Trd4 (k ∷ (b, c, d, e)) ∷ c where Trd4 '(_, _, x, _) = x
type family Fth4 (k ∷ (b, c, d, e)) ∷ c where Fth4 '(_, _, _, x) = x

instance (SingKind k, Eq  (Demote k)) ⇒  D.GEq (Sing ∷ k → Type) where
  geq singl singr
    | fromSing singl ≡ fromSing singr = Just $ U.unsafeCoerce Refl
    | otherwise                       = Nothing

instance (SingKind k, Ord (Demote k)) ⇒  D.GCompare (Sing ∷ k → Type) where
  gcompare singl singr =
    case compare (fromSing singl) (fromSing singr) of
      EQ → U.unsafeCoerce GEQ
      LT → U.unsafeCoerce GLT
      GT → U.unsafeCoerce GGT


-- * Utils
--
lowerShow,  upperShow ∷ Show a ⇒ a → String
lowerShow  = (toLower <$>) ∘ show
upperShow  = (toUpper <$>) ∘ show

lowerShowT, upperShowT, showT ∷ Show a ⇒ a → T.Text
lowerShowT = T.pack ∘ lowerShow
upperShowT = T.pack ∘ upperShow
showT      = T.pack ∘ show

errorT ∷ HasCallStack ⇒ T.Text → a
errorT = error ∘ T.unpack

-- * These turn (1 ∷ Int) into (int 1).
int ∷ Int → Int
int = id

str ∷ String → String
str = id


data Side
  = Bid
  | Ask
  deriving (Eq, Generic, Ord, Show)
deriving instance Show Symbol

$(genSingletons [''Side])

deriving instance Show (SSide a)

type family Oppo (d ∷ Side) ∷ Side where
  Oppo Bid = Ask
  Oppo Ask = Bid

side'char ∷ SSide k → Char
side'char SBid = '>'
side'char SAsk = '<'

type family SideOf (t ∷ k) ∷ Side
side ∷ SingI (SideOf t) ⇒ t → SSide (SideOf t)
side = const sing

data Act (a ∷ Side) where
  Buy  ∷ Act Bid
  Sell ∷ Act Ask


-- * Pretty-printing
--
class PP a where
  pp    ∷ a → T.Text
  brief ∷ a → T.Text
  brief = pp


-- * Currency symbols
--
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
  deriving (Bounded, Enum, Eq, Generic, Ord, Show, Read)

$(genSingletons [''Sym])

deriving instance Show (SSym a)

instance PP Sym where
  pp = T.pack          ∘ upperShow
instance PP (SSym a) where
  pp = T.pack ∘ drop 1 ∘ upperShow


-- * Pair
--
data Pair where
  Pair ∷
    { p'bid ∷ Sym
    , p'ask ∷ Sym
    } → Pair
  deriving (Eq, Generic, Ord, Show)

$(genSingletons [''Pair])

type SPair' a b = SPair ('Pair a b) -- avoid despair

deriving instance Show (SPair a)
instance PP Pair         where pp (Pair  a b) = pp a <> "-" <> pp b
instance PP (SPair' a b) where pp (SPair a b) = pp a <> "-" <> pp b

type family ShowPair (ab ∷ Pair) ∷ ErrorMessage where
  ShowPair ('Pair sa sb) = ShowType sa :<>: Text "-" :<>: ShowType sb


data A'Pair where
  A'Pair ∷ (SingI a, SingI b) ⇒ SPair' a b → A'Pair

instance PP A'Pair where
  pp (A'Pair p) = pp p


-- * Universal Pair projections
--
type family PairOf (t ∷ k) ∷ Pair

type family PairSym (p ∷ Pair) (s ∷ Side) ∷ Sym where
  PairSym ('Pair b a) Bid = b
  PairSym ('Pair b a) Ask = a
type family SymOf  (t ∷ k) (s ∷ Side) ∷ Sym where
  SymOf t s = PairSym (PairOf t) s

-- | SingPairI: enough SingI for 'a' to be a SingI Pair.
type SingPairI (a ∷ Pair) = (SingI a, SingI (PairSym a Bid), SingI (PairSym a Ask))

pair ∷ SingPairI (PairOf t) ⇒ t → SPair (PairOf t)
bid  ∷ SingPairI (PairOf t) ⇒ t → SSym  (SymOf t Bid)
ask  ∷ SingPairI (PairOf t) ⇒ t → SSym  (SymOf t Ask)
pair = const sing
bid  = const sing
ask  = const sing


-- * Pos ≡ (Pair ⋅ Side), namely, what side of a pair we're considering.
--
data Pos = Pos Pair Side deriving (Eq, Ord)

$(genSingletons       [''Pos])
type  SPos' p s = SPos ('Pos p s)

deriving instance Show (SPos p)

type instance PairOf ('Pos pair side) = pair
type instance SideOf ('Pos pair side) = side

type family   PosOf (t ∷ k) ∷ Pos

-- | SingPosI: enough SingI for 'a' to be a SingI Pos.
type SingPosI (a ∷ Pos) = (SingPairI (PairOf a), SingI (SideOf a), SingI a)

pos  ∷ SingPosI (PosOf t) ⇒ t → SPos (PosOf t)
pos  = const sing

-- | Opposition.
type family OpPos   (p ∷ Pos) ∷ Pos  where OpPos   ('Pos pair side) = 'Pos pair (Oppo side)

-- | From- symbol of a position's implied action.
type family PosFrom (p ∷ Pos) ∷ Sym  where PosFrom ('Pos pair side) = PairSym pair side
-- | To- symbol of a position's implied action.
type family PosTo   (p ∷ Pos) ∷ Sym  where PosTo   ('Pos pair side) = PairSym pair (Oppo side)


-- * Path
--
type PConsSane now rest t =
  (SingPosI now,
   PosTo now ~ If (rest :== '[])
                  (PosTo   now)
                  (PosFrom (Head rest)))

data Path (ps ∷ [Pos]) (t ∷ Pos → Type) where
  PNil  ∷
        Path '[]         t
  PCons ∷ PConsSane now rest t ⇒
    { pc't    ∷ t now
    , pc'rest ∷ Path rest t
    } → Path  (now:rest) t

-- instance {-# OVERLAPS #-}                                  Show (Path '[] t) where
--   show PNil           = ""
-- instance {-# OVERLAPPABLE #-} ∀ (t ∷ Pos → Type) (p ∷ Pos) (ps ∷ [Pos]). Show (t p) ⇒ Show (Path ps t) where
-- instance {-# OVERLAPPABLE #-} (ps ~ (p:prs), Show (t p)) ⇒ Show (Path  ps t) where
--   show (PCons x PNil) = show x
--   show (PCons x xs)   = show x
--     <> " :> " <> case xs of
--                    (PCons x PNil) → show x

(>:) ∷ PConsSane now rest t ⇒
  t now → Path rest t → Path (now:rest) t
(>:) = PCons
infixr >:

data P t (p ∷ Pos) where
  P ∷ t → P t p

-- instance Functor (Path now ps) where
--   fmap f (PNil  x)    = PNil  (f x)
--   fmap f (PCons x xs) = PCons (f x) (fmap f xs)
--
-- deriving instance Foldable (Path now ps)

type family PathHead (p ∷ Pos) (ps ∷ [Pos]) ∷ Pos where
  PathHead h   ps      = h
type family PathLast (p ∷ Pos) (ps ∷ [Pos]) ∷ Pos where
  PathLast h  '[]      = h
  PathLast h  (r : rs) = PathLast r rs
type family PathFrom (p ∷ Pos) (ps ∷ [Pos]) ∷ Sym where
  PathFrom h   ps      = PosFrom h
type family PathTo   (p ∷ Pos) (ps ∷ [Pos]) ∷ Sym where
  PathTo   h  '[]      = PosTo   h
  PathTo   h   ps      = PosTo   (Last ps)

mapPath  ∷ (∀ (p' ∷ Pos). SingPosI p' ⇒ t p' → t' p') → Path ps t → Path ps t'
mapPath _ PNil      = PNil
mapPath f PCons{..} = PCons (f pc't) (mapPath f pc'rest)

-- foldPs ∷ (∀ (p ∷ Pos) (p' ∷ Pos). SingPosI p' ⇒ t p' → t' p → t' p') → t' (PsLast p ps) → Ps p ps t → t' (PsLast p ps)
-- foldPs f unit PNil{..}  = f pn't unit
-- foldPs f unit PCons{..} = f pc't (foldPs f unit pc'rest)


--
-- * Non-entirely-type-level types follow
--

-- * Value
--
data Val a where
  Val ∷
    { v'sym ∷ SSym a
    , v'val ∷ Double
    } → Val a
deriving instance Show (Val a)


-- * Rate
--
data Rate pos where
  Rate ∷ SingPosI pos ⇒
    { fromRate ∷ Double
    } → Rate pos
deriving instance Show (Rate p)

type instance SideOf (Rate p) = SideOf p
type instance PairOf (Rate p) = PairOf p
type instance PosOf  (Rate p) = p

instance SingPosI pos ⇒ PP (Rate (pos ∷ Pos)) where
  brief r = T.pack $ printf     "%c %-5.7f"                (side'char $ side r)                (fromRate r)
  pp    r = T.pack $ printf "%s%c%s %-5.7f" (show $ bid r) (side'char $ side r) (show $ ask r) (fromRate r)


-- * Market moves
--
type Rates = DMap SPos Rate

lookup'rate ∷ SingI p ⇒ Rates → SPos p → Rate p
lookup'rate rs p = flip fromMaybe (D.lookup p rs) (error $ "Missing rate for " <> show p)

fill'in'rates ∷ Path ps SPos → Rates → Path ps Rate
fill'in'rates ps rs = mapPath (\_→ lookup'rate rs sing) ps

newtype MoveCostRate =
  MoveCostRate { fromMoveCostRate ∷ Double } deriving (Show)

type Pa0 = 'Pair BTC BCC
type Pa1 = 'Pair BCC USDT
type Pa2 = 'Pair USDT BTC
pa0 ∷ SPair Pa0
pa1 ∷ SPair Pa1
pa2 ∷ SPair Pa2
(pa0, pa1, pa2) = (sing, sing, sing)

type Po0 = 'Pos Pa0 Bid
type Po1 = 'Pos Pa1 Bid
type Po2 = 'Pos Pa2 Bid
po0 ∷ SPos Po0
po1 ∷ SPos Po1
po2 ∷ SPos Po2
(po0, po1, po2) = (sing, sing, sing)

path ∷ Path '[Po0, Po1, Po2] (P Double)
-- path = PCons (P 1.0) $ PCons (P 1.1) $ PCons (P 1.2) PNil
path = P 1.0 >: P 1.1 >: P 1.2 >: PNil

path'rate'factors ∷ MoveCostRate → Path ps Rate → Path ps (P Double)
path'rate'factors mcr p = induce (1.0) p
  where
    rate'op ∷ Double → Rate p → P Double p
    rate'op x r@(Rate rate) =
      case side r of
        SBid → P $ x / rate / fromMoveCostRate mcr
        SAsk → P $ x ⋅ rate / fromMoveCostRate mcr
    induce ∷ Double → Path ps Rate → Path ps (P Double)
    induce _ PNil = PNil
    induce acc (PCons rate xs) =
      PCons pacc' $ induce acc' xs
      where pacc'@(P acc') = rate'op acc rate

-- rate'step'route
--   ∷ (SingI oppos'dir,
--      PosPair pos ~  MovePair move,
--      oppos       ~  PosOppos pos,
--      move        ~ 'Move from dir pair,
--      oppos'rate  ~  PosRate oppos,
--      oppos'dir   ~  Opposite dir) ⇒
--   Rate pair dir → SPos pos → SMove move → Op move → (PosRate oppos)
-- rate'step'route rate _ SMove{..} (Op (Rate _ _ _ r) MoveCostRate{..}) =
--   (,) sing $ Val sing $ case sM'dir of
--                           SBid → v'val / r / fromMoveCostRate
--                           SAsk → v'val ⋅ r / fromMoveCostRate


-- * Projection
--
-- step'moves  ∷ (SingI from, SingI to, SingI pair,
--                MoveTo head ~ MoveFrom next,
--                head ~ MovesFirst moves,
--                next ~ MovesFirst rest,
--                rest ~ MovesTail moves) ⇒
--   SPos' sdir spair →
--   Moves from to (moves ∷ [Move]) Op →
--   (Maybe (Moves (MoveTo head) (MovesTo rest) rest Op),
--    SPos' pair (Opposite (MoveSide (MovesLast moves))),
--    Val to)
-- step'moves v'from  Done                  = (,,) Nothing     sing v'from
-- step'moves v'from (Moves op@Op{..} rest) = (,,) (Just rest) sing $ v'to
--   where (_,   v'to) = step sing v'from sing op 

-- chain'moves ∷ (SingI from, SingI to, SingI dir, SingI pair,
--                MoveTo head ~ MoveFrom next,
--                head ~ MovesFirst moves,
--                next ~ MovesFirst rest,
--                rest ~ MovesTail moves) ⇒
--   Val from → Moves from to (moves ∷ [Move]) Op → Val to
-- chain'moves from Done   = from
-- chain'moves from moves  = case step'moves from moves of 
--   (Nothing, to) → to
--  (Just rest, _, interm) → chain'moves interm rest

-- * Market
--
type Market a b = Market' ('Pair a b) -- Increase the joy / ease the pain of using DataKinds
data Market' (p ∷ Pair) where
  Market ∷ (SingI a, SingI b) ⇒
    { mk'bid  ∷ Rate ('Pos ('Pair a b) Bid)
    , mk'ask  ∷ Rate ('Pos ('Pair a b) Ask)
    } → Market a b
deriving instance Show (Market a b)
-- instance HasPair (Market a b) where
--   type BidSym (Market a b) = a
--   type AskSym (Market a b) = b
--   pair  = pair ∘ mk'bid
instance PP (Market a b) where
  brief (Market b a) = brief b <> " | " <> "" <> " | " <> brief a
  pp    (Market b a) =    pp b <> " | " <> "" <> " | " <>    pp a


-- * Order book
--
-- data Order (k ∷ Side) b a where
--   Order ∷ (SingI a, SingI b) ⇒
--     { rate   ∷ Rate ('Pair b a) d
--     , volume ∷ Double
--     } → Order k b a
-- deriving instance Show (Order k a b)
-- instance HasPair (Order k a b) where
--   type BidSym (Order k a b) = a
--   type AskSym (Order k a b) = b
--   pair  = pair ∘ rate

-- data Book (a ∷ Sym) (b ∷ Sym) where
--   Book ∷ (SingI a, SingI b) ⇒
--     { bids ∷ [Order Bid a b]
--     , asks ∷ [Order Ask a b]
--     } → Book a b
-- deriving instance Show (Book a b)
-- instance HasPair (Book (a ∷ Sym) (b ∷ Sym)) where
--   type BidSym (Book a b) = a
--   type AskSym (Book a b) = b
--   pair (Book _ _)  = SPair sing sing


-- * Trade
--
-- WHY: this doesn't package a and b as existentials, because some upstreams can
--      benefit from more precise types.
-- data Trade a b where
--   Trade ∷
--     { market ∷ Market a b
--     , book   ∷ Book   a b
--     } → Trade a b
-- data A'Trade = ∀ a b. (SingI a, SingI b) ⇒ A'Trade (Trade a b)
-- instance HasPair (Trade a b) where
--   type BidSym (Trade a b) = a
--   type AskSym (Trade a b) = b
--   pair Trade{..} = pair market


-- * combinators
--
-- fromRate ∷ Rate ('Pair a b) dir → Double
-- inverse  ∷ Rate ('Pair a b) dir → Rate ('Pair b a) k
-- median   ∷ Rate Bid '(a, b) → Rate Ask '(a, b) → Rate Median '(a, b)

-- fromRate (Rate r) = r
-- inverse  (Rate r) = Rate (1 / r)
-- median bid (Rate _ symb syma ask) = Rate MedianR symb syma $ (fromRate bid + ask) * 0.5


-- * System parametrisation
--
data Options where
  Options ∷
    { o'logging ∷ Log.Settings
    } → Options
