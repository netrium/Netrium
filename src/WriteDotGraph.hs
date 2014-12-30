-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# OPTIONS_HADDOCK hide #-}
module WriteDotGraph (renderDotGraph, writeDotFile) where

import Data.Tree

labelTree :: Tree a -> Int -> (Tree (Int,a), Int)
labelTree (Node l ts) n = (Node (n,l) ts', n')
  where
    (ts', n') = labelForest ts [] (n+1)
    labelForest []     nts n = (reverse nts, n)
    labelForest (t:ts) nts n = let (nt, n') = labelTree t n
                                in labelForest ts (nt:nts) n'

treeToGraph :: Tree (Int, String) -> ([(Int, String)], [(Int, Int)])
treeToGraph (Node (n, label) ts) =
  let node  = (n, label)
      edges = [ (n, n') | Node (n', _) _ <- ts ]
      (nodes', edges') = unzip (map treeToGraph ts)
   in (node:concat nodes', edges++concat edges')

writeDotFile :: FilePath -> Tree String -> IO ()
writeDotFile file tree = writeFile file (renderDotGraph tree)

renderDotGraph :: Tree String -> String
renderDotGraph tree =
  unlines (
      [header
      ,graphDefaultAtribs
      ,nodeDefaultAtribs
      ,edgeDefaultAtribs]
    ++ map makeNode nodes
    ++ map makeEdge edges
    ++ [footer]
  )
  where
    (nodes, edges) = treeToGraph (fst $ labelTree tree 0)

    makeNode (n,l) = "\t" ++ show n ++ " [label=\"" ++ escape l ++  "\"];"

    makeEdge (n, n') = "\t" ++ show n ++ " -> " ++ show n' ++ "[];"

    escape []        = []
    escape ('\n':cs) = "\\n" ++ escape cs
    escape ('"' :cs) = "\\\"" ++ escape cs
    escape (c   :cs) = c      : escape cs


header = "digraph contract {"
footer = "}"

graphDefaultAtribs = "\tgraph [fontsize=14, fontcolor=black, color=black];"
nodeDefaultAtribs  = "\tnode [label=\"\\N\", width=\"0.75\", shape=ellipse];"
edgeDefaultAtribs  = "\tedge [fontsize=10];"
