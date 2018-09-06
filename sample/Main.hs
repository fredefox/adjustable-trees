{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
module Main (main) where

import           Prelude hiding (lookup, filter)

import           Data.Tree (Tree)
import qualified Data.Tree as Tree
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)

import qualified Control.Monad.ST as ST
import           Control.Monad.ST.Class

import           Pretty
import           Pretty.Orphans ()

import           Data.Tree.Adjustable (toTree, fromTree)
import           Data.Tree.Unsaturated


-- ** Helpers for constructing saturated trees

node ∷ a → [Frozen a] → Frozen a
node c xs = Tree.Node (Right c) xs

leaf ∷ a → Frozen a
leaf a = node a mempty

meta ∷ a → Frozen a
meta c = Tree.Node (Left c) mempty

aTree ∷ Frozen Char
aTree = node 'f'
  [ meta 'r'
  , leaf 'e'
  , node 'd' [leaf 'e', meta 'f', leaf 'o', leaf 'x']
  ]

anUnsatTree ∷ MonadST m ⇒ m (Unsat (World m) Char)
anUnsatTree = fromTree aTree

-- allTheMetasM ∷ ∀ s . ST s [Unsat (World (ST s)) Char]
allTheMetasM ∷ MonadST m ⇒ m [Frozen Char]
allTheMetasM = anUnsatTree >>= allMetas >>= traverse toTree

allTheMetas ∷ [Frozen Char]
allTheMetas = ST.runST allTheMetasM

replacedM ∷ MonadST m ⇒ m [Frozen Char]
replacedM = anUnsatTree >>= replaceHoles candidates
  where
  candidates ∷ [Frozen Char]
  candidates = [leaf 'X', leaf 'B']

replaced ∷ [Frozen Char]
replaced = ST.runST replacedM

main ∷ IO ()
main = putDoc $ prettyVsep t
  where
  t ∷ [Tree String]
  t = fmap showVal <$> replaced

showVal ∷ Value Char → String
showVal = \case
  Left c  → '#':c:[]
  Right c → c:[]
