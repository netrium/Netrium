-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# OPTIONS_HADDOCK hide #-}
module Display (

    module Data.Tree,
    Display(..),
    trimDepth,
    module WriteDotGraph,

    disp, disp',

  ) where

import Control.Monad (void)
import Data.Tree
import System.Process

import WriteDotGraph


class Display a where
  toTree :: a -> Tree String

trimDepth :: Int -> Tree String -> Tree String
trimDepth 0 (Node _ _)  = Node "..." []
trimDepth n (Node l ts) = Node l     (map (trimDepth (n-1)) ts)

-- Utils for use in ghci:

disp :: Display a => a -> IO ()
disp = disp' 8

disp' :: Display a => Int -> a -> IO ()
disp' depth x = do
  writeDotFile "out.dot" (trimDepth depth $ toTree x)
  void $ rawSystem "dot" ["-Tsvg", "-o", "out.svg", "out.dot"]
  void $ rawSystem "eog" ["out.svg"]
