{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DemoContractAST where

import Data.Time
import Data.List (transpose)
import Data.Monoid (Monoid (..))
import System.Locale
import Text.XML.HaXml.Types
import Text.XML.HaXml.XmlContent.Parser

import Common (DiffDateTime, adjustDateTime, DateTime, FeeCalc)
import Calendar

newtype Location = Location String deriving Show
newtype Commodity = Commodity String deriving Show

type Volume = Double
type Price = Double

data Market = Market Commodity Unit Location deriving Show

data OptionDirection = CallOption | PutOption deriving Show

newtype Unit = Unit String deriving Show
newtype Currency = Currency String deriving Show
newtype CashFlowType = CashFlowType String deriving Show

data Product a = Product a DeliverySchedule deriving Show

type SegmentedSchedule = [DeliverySchedule]
newtype DeliverySchedule = DeliverySchedule [DeliveryScheduleBlock] deriving (Monoid, Show)

data DeliveryScheduleBlock = DeliveryScheduleBlock
  { startDateTime :: DateTime
  , endDateTime   :: DateTime
  , deliveryDays  :: [DateTime]
  , deliveryShape :: DeliveryShape
  } deriving Show

deliverySchedule :: DateTime -> DateTime
                 -> Calendar -- ^ What days to deliver on
                 -> DeliveryShape
                 -> DeliverySchedule
deliverySchedule start end cal shape =
    DeliverySchedule [DeliveryScheduleBlock start end days shape]
  where
    days = calendarDaysInPeriod cal (start, end)

newtype DeliveryShape = DeliveryShape [DiffTime] deriving (Monoid, Show)

deliverAtTimeOfDay :: Int -> Int -> DeliveryShape
deliverAtTimeOfDay hour minute = DeliveryShape
  [timeOfDayToTime (TimeOfDay hour minute 0)]

deliverAtMidnight :: DeliveryShape
deliverAtMidnight = deliverAtTimeOfDay 0 0

complexDeliveryShape :: [DeliveryShape] -> DeliveryShape
complexDeliveryShape = mconcat

data Contract
  = NamedContract String Contract
  | AndContract [Contract]
  | GiveContract Contract
  | ProductBasedContract ProductBasedContract
  | OptionContract
      { optionDirection :: OptionDirection
      , premium         :: PremiumPrice
      , exerciseDetails :: ExerciseDetails
      , underlying      :: Contract
      }
  deriving Show

data ProductBasedContract
  = PhysicalContract Double (Product Market)
  | FinancialContract Double (Product Currency) (Maybe CashFlowType)
  deriving Show

type StrikePrice  = (Double, Currency)
type PremiumPrice = (Double, Currency)

data ExerciseDetails
  = European StrikePrice (DateTime, DateTime)
  | American StrikePrice (DateTime, DateTime)
  | Bermudan StrikePrice [(DateTime, DateTime)] Int
  deriving Show

--------------------------------------------------------------------------------


forward :: FeeCalc                            -- ^Fees
        -> Market                             -- ^Market, e.g. NBP Gas
        -> Volume                             -- ^Notional volume
        -> Price                              -- ^Price
        -> Currency                           -- ^Payment currency
        -> CashFlowType                       -- ^Cashflow type
        -> DeliverySchedule                   -- ^Schedule for physical settlement
        -> DeliverySchedule                   -- ^Schedule for financial settlement
        -> Contract
forward _fee market vol pr cur cft pSch fSch = NamedContract "Forward" $
  AndContract [ ProductBasedContract physicalLeg
              , GiveContract $ ProductBasedContract financialLeg]
 where
  physicalLeg      = PhysicalContract vol physicalProduct
  physicalProduct  = Product market pSch
  financialLeg     = FinancialContract (vol * pr) financialProduct (Just cft)
  financialProduct = Product cur fSch


-- * Option template
-- | Basic option template. Flexibility is achieved through 'ExerciseDetails'.
option :: ExerciseDetails           -- ^Details of when and at what price the option can be exercised
       -> OptionDirection           -- ^Direction of the option (call or put)
       -> PremiumPrice
       -> Contract                  -- ^Underlying asset
       -> Contract
option exerciseDetails direction premium underlyingContract =
  OptionContract direction premium exerciseDetails underlyingContract

namedContract :: String -> Contract -> Contract
namedContract = NamedContract

europeanExercise :: (DateTime, DateTime) -> StrikePrice -> ExerciseDetails
europeanExercise = flip European

americanExercise :: (DateTime, DateTime) -> StrikePrice -> ExerciseDetails
americanExercise = flip American

allOf :: [Contract] -> Contract
allOf = AndContract


commoditySpreadOption ::
                   [( Market
                   , Volume
                   , Price, Currency, CashFlowType
                   , SegmentedSchedule
                   , FeeCalc
                   )]                       -- ^List of underlying legs
               -> DiffDateTime                        -- ^Exercise start date offset (relative to leg)
               -> DiffDateTime                        -- ^Exercise stop date offset (relative to leg)
               -> OptionDirection                     -- ^Option direction (put or call)
               -> StrikePrice                         -- ^Strike price of the option
               -> CashFlowType                        -- ^Cashflow type of the strike price
               -> PremiumPrice
               -> Contract
commoditySpreadOption legs exerciseDiffTimeStart exerciseDiffTimeStop opDir strikePrice cftype premium =
   allOf
     [ legOption groupedLeg $
         allOf [ forward fee m vol pr cur cft seg seg
               | (m, vol, pr, cur, cft, seg, fee) <- groupedLeg ]
     | groupedLeg <- groupedLegs ]

 where
   groupedLegs :: [[(Market, Volume, Price, Currency, CashFlowType, DeliverySchedule, FeeCalc)]]
   groupedLegs =
     transpose
       [ [ (m, vol, pr, cur, cft, seg, fee) | seg <- sch ]
       | (m, vol, pr, cur, cft, sch, fee) <- legs ]

   legOption :: [(Market, Volume, Price, Currency, CashFlowType, DeliverySchedule, FeeCalc)] -> Contract -> Contract
   legOption groupedLeg underlying =
       option exerciseDetails opDir premium underlying

     where
       exerciseDetails = europeanExercise exerciseTime strikePrice
       exerciseTime    = ( adjustDateTime earliestDeliveryTime exerciseDiffTimeStart
                         , adjustDateTime earliestDeliveryTime exerciseDiffTimeStop 
                         )
         where
           earliestDeliveryTime =
             minimum [ earliestDelivery seg | (_, _, _, _, _, seg, _) <- groupedLeg ]

earliestDelivery :: DeliverySchedule -> DateTime
earliestDelivery (DeliverySchedule blocks) =
  minimum [ day | block <- blocks, day <- deliveryDays block]

--------------------------------------------------------------------------------

toXml :: XmlContent' a => Bool -> a -> Document ()
toXml _ value =
  Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing))
                   [] Nothing [])
           emptyST
           ( case toContents' value of
               []             -> Elem "empty" [] []
               [CElem e ()]   -> e
               (CElem _ ():_) -> error "too many XML elements in document" )
           []

class XmlContent' a where
  toContents' :: a -> [Content ()]

instance XmlContent' Contract where
  toContents' (NamedContract name contract) =
    [ CElem (Elem "Contract" [mkAttr "type" name] (toContents' contract)) () ]
  toContents' (AndContract contracts) =
    [ mkElemC "And" (concatMap toContents' contracts) ]
  toContents' (GiveContract contract) =
    [ mkElemC "Give" (toContents' contract) ]
  toContents' (ProductBasedContract contract) =
    toContents' contract
  toContents' (OptionContract dir (premiumQty, premiumCcy) det und) =
    [ mkElemC "Option" $ concat
      [ toContents' dir
      , toContents' det
      , [ mkElemC "Premium" $ concat
          [ [ mkElemC "Quantity" (toText . show $ premiumQty) ]
          , toContents' premiumCcy
          ]
        ]
      , [ mkElemC "Underlying" (toContents' und) ]
      ]
    ]

instance XmlContent' OptionDirection where
  toContents' CallOption = [mkElemC "OptionDirection" $ toText "Call"]
  toContents' PutOption  = [mkElemC "OptionDirection" $ toText "Put"]

instance XmlContent' ExerciseDetails where
  toContents' (European strike window) =
    [ CElem (Elem "ExerciseDetails" [mkAttr "type" "European"] $ concat
      [ [ mkExerciseWindow [window]
        , mkStrikePrice strike
        , mkElemC "Premium" [] ]
      ]) () ]
  toContents' (American strike window) =
    [ CElem (Elem "ExerciseDetails" [mkAttr "type" "American"] $ concat
      [ [ mkExerciseWindow [window]
        , mkStrikePrice strike
        , mkElemC "Premium" [] ]
      ]) () ]
  toContents' (Bermudan strike windows _limit) =
    [ CElem (Elem "ExerciseDetails" [mkAttr "type" "American"] $ concat
      [ [ mkExerciseWindow windows
        , mkStrikePrice strike
        , mkElemC "Premium" [] ]
      ]) () ]

mkStrikePrice (strikeQty, strikeCcy) =
  mkElemC "StrikePrice" $ concat
    [ [ mkElemC "Quantity" (toText . show $ strikeQty) ]
    , toContents' strikeCcy
    ]

mkExerciseWindow :: [(DateTime, DateTime)] -> Content ()
mkExerciseWindow windows = 
  mkElemC "ExerciseWindow" $ map mkExerciseWindowElement windows

mkExerciseWindowElement :: (DateTime, DateTime) -> Content ()
mkExerciseWindowElement (startDateTime, endDateTime) =
  mkElemC "ExerciseWindowElement" 
    [ mkElemC "StartDateTime" $ toText . showDate $ startDateTime
    , mkElemC "EndDateTime" $ toText . showDate $ endDateTime
    ]

instance XmlContent' Currency where
  toContents' (Currency ccy) = [mkElemC "Currency" (toText ccy)]

instance XmlContent' ProductBasedContract where
  toContents' (PhysicalContract qty prod) =
    [ CElem (Elem "Contract" [mkAttr "type" "Physical"] $ concat
      [ toContents' prod
      , [ mkElemC "Quantity" $ toText . show $ qty ]
      ]) () ]
  -- TODO: Is there a need for the mCashFlowType here?
  toContents' (FinancialContract qty prod _mCashFlowType) =
    [ CElem (Elem "Contract" [mkAttr "type" "Financial"] $ concat
      [ toContents' prod
      , [ mkElemC "Quantity" $ toText . show $ qty ]
      ]) () ]

instance XmlContent' a => XmlContent' (Product a) where
  toContents' (Product market schedule) =
    [ mkElemC "Product" $ toContents' market ++ toContents' schedule ]

instance XmlContent' Market where
  toContents' (Market commodity unit location) =
    [ mkElemC "Market" $ concat
      [ toContents' commodity
      , toContents' unit
      , toContents' location
      ]
    ]

instance XmlContent' Commodity where
  toContents' (Commodity commodity) = [mkElemC "Commodity" $ toText commodity ]

instance XmlContent' Unit where
  toContents' (Unit unit) = [mkElemC "Unit" $ toText unit ]

instance XmlContent' Location where
  toContents' (Location location) = [mkElemC "Location" $ toText location ]

instance XmlContent' DeliverySchedule where
  toContents' (DeliverySchedule scheds) =
    [ mkElemC "DeliverySchedule" $ concatMap toContents' scheds ]

instance XmlContent' DeliveryScheduleBlock where
  toContents' (DeliveryScheduleBlock startDateTime endDateTime days shape) =
    [ mkElemC "DeliveryScheduleElement" $ concat
      [ [ mkElemC "StartDateTime" $ toText . showDate $ startDateTime ]
      , [ mkElemC "EndDateTime" $ toText . showDate $ endDateTime ]
      , map (mkElemC "DeliveryDay" . toText . showDay) days
      , toContents' shape
      ]
    ]

instance XmlContent' DeliveryShape where
  toContents' (DeliveryShape times) =
    [ mkElemC "DeliveryShape" $ concatMap
        (return . mkElemC "DeliveryShapeElement" . toText . showTime) times
    ]

showDate = formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S"
showDay  = formatTime defaultTimeLocale "%d/%m/%Y"
showTime = formatTime defaultTimeLocale "%H:%M:%S" . timeToTimeOfDay
