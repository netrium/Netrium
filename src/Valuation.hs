-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
-- Module for valuation semantics
module Valuation where

import Prelude hiding (or, min, abs, negate, not, read, until)
import Contract hiding (and, max)
import Observable hiding (max, eval)

import Numeric

import System.Process
import System.Exit

-- *Value Processes
-- **The basics
-- |Type for value processes
newtype PR a = PR { unPr :: [RV a] } deriving Show

-- |Random variables
type RV a = [a]

-- **Value process helpers
-- |Truncates a (possibly infinite) value process
takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

-- |Determines the number of time steps in a value process. Only terminates for finite value processes
horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

-- |Returns True if every value in a value process is true, false otherwise. Only terminates for finite value processes.
andPr :: PR Bool -> Bool
andPr (PR rvs) = and (map and rvs)

-- **Value process lifting
-- |Lift functions wih a single argument
liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

-- |Lift functions with 2 arguments
lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

-- |Lift functions for binary operations
lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

-- |Lift functions with 3 arguments
lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

-- |A version of zipWith that handles input lists of different lengths
--
-- This is used to support lifted binary operations such as (+)
zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (x:xs) (y:ys)     = f x y : zipWithAll f xs ys
zipWithAll _ xs@(_:_) []       = xs
zipWithAll _ []       ys@(_:_) = ys
zipWithAll _ _        _        = []

-- |To use Num operations on PR
instance Num a => Num (PR a) where
   fromInteger i = bigK (fromInteger i)
   (+) = lift2PrAll (+)
   (-) = lift2PrAll (-)
   (*) = lift2PrAll (*)
   abs = liftPr  abs
   signum = liftPr signum

-- |To use Ord operations on PR
instance Ord a => Ord (PR a) where
   max = lift2Pr (Prelude.max)
   compare = Prelude.compare

-- |To use Equality operations on PR
instance Eq a => Eq (PR a) where
   (PR a) == (PR b) = a == b

-- *Models
-- |A model has a start date/time and an exchange rate model
data Model = Model {
   modelStart :: Time,                              -- ^Start date and time for the model
   exch       :: Currency -> Currency -> PR Double      -- ^Exchange rate model
   }

-- |A simple model with a constant exchange rate model
simpleModel :: Time -> Model
simpleModel modelDate = Model {
   modelStart = modelDate,
   exch       = exchModel
   }
   where
      -- Exchange rate model
      exchModel :: Currency -> Currency -> PR Double
      exchModel _k1 _k2 = PR (konstSlices 1)

-- |Each currency has different parameters for the interest rate model. Since the model is not realistic, these parameters are currently entirely arbitrary.
rateModels :: [(Currency, PR Double)]
rateModels = [((Currency "CHF"), rates 7   0.8)
             ,((Currency "EUR"), rates 6.5 0.25)
             ,((Currency "GBP"), rates 8   0.5)
             ,((Currency "KYD"), rates 11  1.2)
             ,((Currency "USD"), rates 5   1)
             ,((Currency "ZAR"), rates 15  1.5)]

-- |Function to pick an exchange rate model from the above list
rateModel :: Currency -> PR Double
rateModel k =
  case lookup k rateModels of
    Just x -> x
    Nothing -> error $ "rateModel: currency not found " ++ (show k)

-- *Process primitives
-- |Constant process
bigK :: a -> PR a
bigK x = PR (konstSlices x)

-- |Generate an infinite list
konstSlices :: t -> [[t]]
konstSlices x = nextSlice [x]
  where nextSlice sl = sl : (nextSlice (x:sl))

-- This needs to be changed as we are dealing with proper dates
--datePr :: PR DateTime
--datePr = PR $ timeSlices [time0]

--timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

-- |Evaluate a condition at date T
condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

-- |The primitive (disc t k) maps a real-valued random variable at date T, expressed in currency k, to its \"fair\" equivalent stochastic value process in the same currency k.
--
-- A simplifying assumption is that at some point, the boolean-valued process becomes True for an entire RV. This provides a simple termination condition for the discounting process.
disc :: Currency -> (PR Bool, PR Double) -> PR Double
disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)
   where
       discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
       discCalc (bRv:bs') (pRv:ps) (rateRv:rs') =
         if and bRv -- test for horizon
           then [pRv]
           else let rest@(nextSlice:_) = discCalc bs' ps rs'
                    discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                    thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
                                  bRv pRv discSlice
                in thisSlice : rest
       discCalc _ _ _ = []

-- |Given a boolean-valued process o, the primitive absorbk(o,p) transforms the real-valued process p, expressed in currency k, into another real-valued process. For any state, the result is the expected value of receiving p's value if the region o will never be True, and receiving zero in the contrary. In states where o is True, the result is therefore zero
absorb :: Currency -> (PR Bool, PR Double) -> PR Double
absorb _ (PR bSlices, PR rvs) =
   PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                bSlices rvs

-- |Not currently implemented. The paper describes the following as a possible algorithm:
--
-- - take the final column of the tree (horizon),
--
-- - discount it back one time step,
--
-- - take the maximum of that column with the corresponding column of the original tree,
--
-- - then repeat that process all the way back to the root.
--
-- snellk(o,p) is the smallest process q (under an ordering relation mention briefly at the end of B:4.6) such that:
--
-- @
-- forall o' . (o => o') => q >= snellk(o',q)
-- @
snell :: Currency -> (PR Bool, PR Double) -> PR Double
snell _ (PR _, prd) = prd -- stub, doesn't do anything

-- *Lattices
-- **Simple calculation
-- |Calculates a previous slice in a lattice by averaging each adjacent pair of values in the specified slice
prevSlice :: RV Double -> RV Double
prevSlice [] = []
prevSlice (_:[]) = []
prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

-- |Constructs a lattice containing possible interest rates given a starting rate and an increment per time step. This \"unrealistically regular\" model matches that shown in B:Fig.8. However, it is so simple that some interest rates go negative after a small number of time steps. A better model is needed for real applications. Don't use this to model your retirement fund!
rates :: Double -> Double -> PR Double
rates rateNow delta = PR $ makeRateSlices rateNow 1
   where
       makeRateSlices rateNow' n = (rateSlice rateNow' n) : (makeRateSlices (rateNow'-delta) (n+1))
       rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

-- **Probability calculation
-- |Each node in a value process lattice is associated with a probability.
--
-- \"...in our very simple setting the number of paths from the root to the node is proportional to the probability that the variable will take that value.\"
probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
     where
       probabilities :: [RV Integer] -> [RV Double]
       probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls
       probabilities [] = []

-- To calculate the number of paths to each node in a lattice, simply add the number of paths to the pair of parent nodes. This needs to work with Integers as opposed to Ints, because: findIndex (\sl -> maximum sl > (fromIntegral (maxBound::Int))) pathCounts ==> Just 67
       pathCounts :: [RV Integer]
       pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))

-- **Expected value
-- |The code for absorb above does not obviously deal with the expected value mentioned in the spec. This is because the expected value of each random variable is implicit in the value process lattice representation: each node in the lattice is associated with a probability, and the expected value at a particular date is simply the sum of the product of the value at each node and its associated probability. The following functions implement this calculation.
expectedValue :: RV Double -> RV Double -> Double
expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

expectedValuePr :: PR Double -> [Double]
expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

-- *Valuation semantics
-- **Valuation semantics for contracts
-- |Evaluate a contract at a given time
evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model _ ex) k = eval
   where eval Zero                       = bigK 0
         eval (One (Financial k2 _ _))   = ex k k2
         eval (Give c)                   = -(eval c)
         eval (o `Scale` c)              = (evalO o) * (eval c)
         eval (c1 `And` c2)              = (eval c1) + (eval c2)
         eval (Or _ c1 c2)               = max (eval c1) (eval c2)
         eval (Cond o c1 c2)             = condPr (evalO o) (eval c1) (eval c2)
         eval (When o c)                 = disc k (evalO o, eval c)
         eval (Until o c)                = absorb k (evalO o, eval c)
         eval (Anytime _ o c)            = snell k (evalO o, eval c)
         eval (Party _ c)                = eval c
         eval (Read {})                  = error "valuation of 'Read' not implemented"
         eval (One (Physical {}))        = error "unable to value contracts for physical tradeables"

-- **Valuation semantics for observables
-- |Evaluate a constant observable
evalO :: Obs a -> PR a
evalO (Const v) = bigK v
evalO _         = error "cannot evaluate non-constant observables"

-- *Functions for Graphviz output
-- |This code generates graphs which represent a value process lattice. Currently assumes Double values, constrained by showNode's formatting of the value.
--
-- Write out tree as Dot file and run Dot to generate image:
latticeImage :: PR Double -> String -> String -> IO ExitCode
latticeImage pr baseName imageType =
   do writeTreeAsDot baseName pr
      runDot baseName imageType

-- |Supports interactive display of generated Dot code.
printTree :: PR Double -> IO ()
printTree pr = mapM_ putStrLn (dotGraph (prToDot pr))

-- |Write a value process out as a Dot file.
writeTreeAsDot :: String -> PR Double -> IO ()
writeTreeAsDot baseName pr = writeFile (baseName ++ dotExt) $ unlines (dotGraph (prToDot pr))

-- |Run Dot on a file with the specified base name, and generate a graphic file with the specified type.
runDot :: String -> String -> IO ExitCode
runDot baseName fileType =
   system $ concat ["dot -T", fileType,
                    " -o ", baseName, ".", fileType, " ",
                    baseName, dotExt]

-- |Convert a 'PR' 'Double' to a list of dot node relationships.
prToDot :: PR Double -> [String]
prToDot (PR rvs) = rvsToDot rvs

-- |Convert lattice to list of dot node relationships.
rvsToDot :: [RV Double] -> [String]
rvsToDot rvs = let numberedRvs = assignIds rvs 1
                in showNodes numberedRvs ++ treeToDot numberedRvs
dotExt :: String
dotExt = ".dot"

-- |Number each of the nodes in a lattice.
assignIds :: [RV a] -> Int -> [RV (Int, a)]
assignIds [] _ = []
assignIds (rv:rvs) n = numberList (reverse rv) n : assignIds rvs (n + length rv)

numberList :: [a] -> Int -> [(Int, a)]
numberList l n = zip [n .. n + length l] l

-- |showNodes returns a list of \"primary\" Dot representations of numbered 'RV' nodes, with each node's value specified as the node's label. These nodes can then be referenced repeatedly in the generated Dot code without specifying a label.
showNodes :: [RV (Int, Double)] -> [String]
showNodes numberedRvs = concatMap showSlice (numberList numberedRvs 0)
   where showSlice (n, sl) = ("subgraph Slice" ++ show n ++ " { rank=same")
                             : (map (\(n',s) -> show n' ++ nodeLabel s) sl)
                             ++ ["SL" ++ (show n) ++ " [label=\"" ++ show n ++ "\" style=solid peripheries=0] }"]

nodeLabel :: Double -> String
nodeLabel s = " [label=\"" ++ (showFFloat (Just 2) s "\"]")

-- |Generate Dot code for relationships between numbered 'RV' nodes.
treeToDot :: [RV (Int, a)] -> [String]
treeToDot [] = []
treeToDot [_] = []
treeToDot (a:b:rest) = dotJoin a (take (length a) b)
                     ++ dotJoin a (tail b)
                     ++ treeToDot (b:rest)

dotJoin :: RV (Int, a) -> RV (Int, a) -> [String]
dotJoin a b = zipWith (\(m,_) (n,_) -> (show m) ++ " -- " ++ (show n)) a b

dotGraph :: [String] -> [String]
dotGraph body = dotGraphHdr ++ (map formatDotStmt body) ++ ["}"]

dotGraphHdr :: [String]
dotGraphHdr = ["graph contract_lattice {"
                 ,"  rankdir=LR;"
                 ,"  dir=none;"
                 ,"  node [style=filled color=pink shape=box fontsize=10 width=0.5 height=0.4];"]

formatDotStmt :: String -> String
formatDotStmt s = "  " ++ s ++ ";"
