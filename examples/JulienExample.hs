import Contract
import Common
import Options
import Data.List (transpose)

commoditySpreadOption :: ChoiceId
                -> [( Market
                    , Volume
                    , Price, Currency, CashFlowType
                    , SegmentedSchedule
                    , FeeCalc )]
                -> DiffDateTime
                -> OptionDirection
                -> StrikePrice
                -> Contract
commoditySpreadOption cid legs optionDiffTime opDir strikePrice =
    allOf
      [ legOption groupedLeg $ \strikePrice ->
          -- do something here with strikePrice, else it's never used!
          allOf [ forward fee m pr cur cft vol seg
                | (m, pr, cur, cft, vol, seg, fee) <- groupedLeg ]
      | groupedLeg <- groupedLegs ]

  where
    groupedLegs =
      transpose
        [ [ (m, pr, cur, cft, vol, seg, fee) | seg <- sch ]
        | (m, pr, cur, cft, vol, sch, fee) <- legs ]

    legOption groupedLeg =
        option cid (europeanExercise (optionTime groupedLeg) strikePrice) opDir

    optionTime groupedLeg =
        adjustDateTime earliestDeliveryTime optionDiffTime
      where
        earliestDeliveryTime =
          minimum [ t | (_, _, _, _, _, (t:_), _) <- groupedLeg ]
