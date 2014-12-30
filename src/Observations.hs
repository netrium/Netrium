-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# LANGUAGE TypeSynonymInstances #-}
module Observations where

import Contract
import DecisionTree
import Observable (VarName)
import ObservableDB (ObservableType(..))
import XmlUtils

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Function
import Text.XML.HaXml.XmlContent


type Observations a = Map VarName  (TimeSeries a)
type Choices      a = Map ChoiceId (TimedEvents a)


-- ---------------------------------------------------------------------------
-- * Time series
-- ---------------------------------------------------------------------------

data TimeSeries a = SeriesEntry Time a (TimeSeries a)
                  | SeriesEnds  Time
                  | SeriesUnbounded
  deriving (Show, Read)

lookupTimeSeries :: TimeSeries a -> Time -> Maybe a
lookupTimeSeries ts0 time = case ts0 of
    SeriesEntry t v ts
      | time >= t      -> go v ts
    _                  -> Nothing
  where
    go v (SeriesEntry t' v' ts')  | time < t'  = Just v
                                  | otherwise  = go v' ts'
    go v (SeriesEnds t')          | time < t'  = Just v
                                  | otherwise  = Nothing
    go v SeriesUnbounded                       = Just v

pruneTimeSeries :: Time -> TimeSeries a -> TimeSeries a
pruneTimeSeries time (SeriesEntry _t _v (SeriesEntry t' v' ts'))
  | time >= t' = pruneTimeSeries time (SeriesEntry t' v' ts')
pruneTimeSeries _time ts = ts

lookupChoice :: Map ChoiceId (TimedEvents a) -> ChoiceId -> Time -> Maybe a
lookupChoice choices cid time = do
  tvs <- Map.lookup cid choices
  lookupTimedEvent tvs time

timeSeriesEvents :: TimeSeries a -> TimedEvents (Maybe a)
timeSeriesEvents = TEs . go
  where
    go (SeriesEntry t v ts) = (t, Just v)  : go ts
    go (SeriesEnds  t)      = (t, Nothing) : []
    go SeriesUnbounded      = []

-- ---------------------------------------------------------------------------
-- * Timed events
-- ---------------------------------------------------------------------------

newtype TimedEvents a = TEs [(Time, a)]
  deriving (Show, Read, Eq)

unTEs (TEs x) = x

instance Functor TimedEvents where
  fmap f (TEs tes) = TEs [ (t,f e) | (t,e) <- tes ]

instance Monoid a => Monoid (TimedEvents a) where
  mempty        = TEs []
  mappend as bs =
      fmap mappendMergeResult (mergeEvents as bs)
    where
      mappendMergeResult (OnlyInLeft  a)   = a
      mappendMergeResult (InBoth      a b) = a `mappend` b
      mappendMergeResult (OnlyInRight   b) =             b

  -- mconcat = --TODO: optimise mconcat to do more balanced merges

mapAccumTS :: (acc -> x -> (acc, y))
           ->  acc -> TimedEvents x
           -> (acc,   TimedEvents y)
mapAccumTS f a0 (TEs tes) = let (a',     tes') = mapAccumL f' a0 tes
                             in (a', TEs tes')
  where
    f' a (t,x) = let (a', y) = f a x in (a', (t,y))


lookupTimedEvent :: TimedEvents a -> Time -> Maybe a
lookupTimedEvent (TEs tes) = go tes
  where
    go []           _                = Nothing
    go ((t,e):tes') time | time > t  = go tes' time
                         | time == t = Just e
                         | otherwise = Nothing

-- | Insert an event into a TimedEvents series.
--
-- This event is placed before the other simultaneous events in the sequence.
--
insertEventBefore :: Time -> a -> TimedEvents a -> TimedEvents a
insertEventBefore time e = TEs . insertBy (compare `on` fst) (time, e) . unTEs

-- | Insert an event into a TimedEvents series.
--
-- This event is placed after the other simultaneous events in the sequence.
--
insertEventAfter :: Time -> a -> TimedEvents a -> TimedEvents a
insertEventAfter time e =
    TEs . insertAfterBy (compare `on` fst) (time, e) . unTEs
  where
    insertAfterBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
    insertAfterBy _   x []         = [x]
    insertAfterBy cmp x ys@(y:ys') =
        case cmp x y of
          LT -> x : ys
          _  -> y : insertAfterBy cmp x ys'

pruneTimedEvents :: Time -> TimedEvents a -> TimedEvents a
pruneTimedEvents time = TEs . dropWhile (\(t,_) -> t < time) . unTEs

mergeEvents :: TimedEvents a -> TimedEvents b -> TimedEvents (MergeResult a b)
mergeEvents (TEs as) (TEs bs) =
    TEs (map combine (mergeBy (\(t,_) (t',_) -> compare t t') as bs))
  where
    combine (OnlyInLeft  (t,a)      ) = (t, OnlyInLeft a)
    combine (InBoth      (t,a) (_,b)) = (t, InBoth     a b)
    combine (OnlyInRight       (t,b)) = (t, OnlyInRight  b)

-- For simultaneous events, ones from the second stream are placed second.
--
mergeEventsBiased :: TimedEvents a -> TimedEvents a -> TimedEvents a
mergeEventsBiased as bs =
    TEs . foldr shuffle [] . unTEs $ mergeEvents as bs
  where
    shuffle (t, OnlyInLeft  a  ) rest = (t, a) :          rest
    shuffle (t, InBoth      a b) rest = (t, a) : (t, b) : rest
    shuffle (t, OnlyInRight   b) rest =          (t, b) : rest


-- ---------------------------------------------------------------------------
-- * Merging
-- ---------------------------------------------------------------------------


-- | Generic merging utility. For sorted input lists this is a full outer join.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b

-- ---------------------------------------------------------------------------
-- * XML instances
-- ---------------------------------------------------------------------------

data ObservationSeries = ObservationsSeriesBool   String (XMLTimeSeries Bool)
                       | ObservationsSeriesDouble String (XMLTimeSeries Double)

instance HTypeable ObservationSeries where
  toHType _ = Defined "ObservationSeries" [] []

instance XmlContent ObservationSeries where
  parseContents = do
    e@(Elem t _ _) <- element ["ObservationSeries"]
    commit $ interior e $ case t of
      "ObservationSeries" -> do
        seriesType <- attrRead "type" e
        seriesVar  <- attrStr  "var"  e
        case seriesType of
          Double -> liftM (ObservationsSeriesDouble seriesVar) parseContents
          Bool   -> liftM (ObservationsSeriesBool   seriesVar) parseContents

  toContents (ObservationsSeriesBool var ts) =
    [mkElemAC "ObservationSeries" [("type", str2attr "Bool")
                                  ,("var",  str2attr var)]
                                  (toContents ts)]
  toContents (ObservationsSeriesDouble var ts) =
    [mkElemAC "ObservationSeries" [("type", str2attr "Double")
                                  ,("var",  str2attr var)]
                                  (toContents ts)]


data ChoiceSeries = ChoiceSeries (XMLTimedEvents Choice)
data Choice       = OrChoice      ChoiceId Bool
                  | AnytimeChoice ChoiceId

instance HTypeable ChoiceSeries where
  toHType _ = Defined "ChoiceSeries" [] []

instance HTypeable Choice where
  toHType _ = Defined "Choice" [] []

instance XmlContent ChoiceSeries where
  parseContents = do
    e@(Elem t _ _) <- element ["Choices"]
    commit $ interior e $ case t of
      "Choices" -> liftM ChoiceSeries parseContents

  toContents (ChoiceSeries cs) = [mkElemC "Choices" (toContents cs)]

instance XmlContent Choice where
  parseContents = do
    e@(Elem t _ _) <- element ["Choice"]
    commit $ interior e $ case t of
      "Choice" -> do
        cid <- attrStr "choiceid" e
        content <- parseContents
        case content of
          Nothing -> return (AnytimeChoice cid)
          Just m  -> return (OrChoice cid m)

  toContents (OrChoice cid m)    =
    [mkElemAC "Choice" [("choiceid", str2attr cid)] (toContents m)]
  toContents (AnytimeChoice cid) =
    [mkElemAC "Choice" [("choiceid", str2attr cid)] []]


data Timed a = Timed Time a
data SeriesEnd = Unbounded | Bounded Time

instance HTypeable SeriesEnd where
  toHType _ = Defined "SeriesEnd" [] []

instance XmlContent SeriesEnd where
  parseContents = do
    e@(Elem t _ _) <- element ["SeriesUnbounded", "SeriesEnds"]
    commit $ interior e $ case t of
      "SeriesUnbounded" -> return Unbounded
      "SeriesEnds"      -> liftM Bounded parseContents

  toContents Unbounded   = [mkElemC "SeriesUnbounded" []]
  toContents (Bounded t) = [mkElemC "Bounded" (toContents t)]


instance HTypeable (Timed a) where
  toHType _ = Defined "SeriesEntry" [] []

instance XmlContent a => XmlContent (Timed a) where
  parseContents = inElement "SeriesEntry" $
                    liftM2 Timed parseContents parseContents
  toContents (Timed t a) = [mkElemC "SeriesEntry" (toContents t ++ toContents a)]

type XMLTimeSeries a = ([Timed a], SeriesEnd)
type XMLTimedEvents a = [Timed a]

toTimedEvents :: XMLTimedEvents a -> TimedEvents a
toTimedEvents xs = TEs $ map (\ (Timed t a) -> (t, a)) xs

fromTimedEvents :: TimedEvents a -> XMLTimedEvents a
fromTimedEvents (TEs xs) = map (\ (t, a) -> Timed t a) xs

toTimeSeries :: XMLTimeSeries a -> TimeSeries a
toTimeSeries (xs, mt) = foldr cons (nil mt) xs
  where
    cons (Timed t a) r = SeriesEntry t a r
    nil Unbounded      = SeriesUnbounded
    nil (Bounded t)    = SeriesEnds t

fromTimeSeries :: TimeSeries a -> XMLTimeSeries a
fromTimeSeries (SeriesUnbounded)   = ([], Unbounded)
fromTimeSeries (SeriesEnds t)      = ([], Bounded t)
fromTimeSeries (SeriesEntry t a r) = case fromTimeSeries r of
                                       (xs, e) -> (Timed t a : xs, e)
