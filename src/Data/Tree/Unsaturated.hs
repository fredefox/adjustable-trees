-- | An implementation of unsaturated trees.
{-# Language StrictData, Strict #-}
{-# OPTIONS_GHC -Wall #-}
module Data.Tree.Unsaturated
  (
  -- * Unsaturated trees.
    Value, Frozen, Unsat
  , allMetas
  , replaceHoles
  , replace
  -- * Saturated trees
  , Sat
  , saturate
  ) where

import           Data.Tree (Tree)
import qualified Data.Tree as Tree

import           Control.Monad.ST.Class
import           Control.Monad ((>=>), join)

import           Data.Tree.Adjustable


-- * Unsaturated trees

-- | A value in an unsaturated tree.  Either a meta ('Left') or an
-- "actual" node ('Right').
type Value a = Either a a

-- | An unsaturated tree is a mutable tree with either metas or
-- "actual" values.  It is an invariant (not expressed in the type of
-- 'Unsat') that a meta does not have children.
--
-- Use 'toTree' / 'fromTree' to convert to- and from this type.
type Unsat s a = R s (Value a)

-- | Find all references to trees with the given label.
allMetas
  ∷ ∀ m a
  . MonadST m
  ⇒ Unsat (World m) a
  → m [Unsat (World m) a]
allMetas = findDescendents p
  where
  p ∷ Value a → Bool
  p = either (const True) (const False)

-- | @'replaceHoles' xs@ replaces all "holes" in a mutable tree with
-- the candidates in @xs@.
replaceHoles
  ∷ ∀ m a
  . MonadST m
  ⇒ [Tree (Value a)]
  → Unsat (World m) a
  → m [Tree (Value a)]
replaceHoles ts root = do
  ts' ← traverse (fromTree >=> readR) ts
  forEachR (go ts') root
  where
  go ∷ [Node (World m) (Value a)] → Unsat (World m) a → m [Tree (Value a)]
  -- @t@ is the current node.
  go ts' t = do
    v ← readR t
    if isMeta v
    then traverse (replace root t) ts'
    else pure mempty

-- | @'replace' t t0 u@ replaces the sub tree @t0@ with @u@ and
-- "freezes" @t@.  @t0@ is not a sub tree of @t@ then @'replace' t t0 u
-- ~ 'toTree' t@.
replace
  ∷ MonadST m
  ⇒ Unsat (World m) a
  → Unsat (World m) a
  → Node (World m) (Value a)
  → m (Tree (Value a))
replace root t curr = do
  prev ← readR t
  writeR curr t
  rt ← toTree root
  writeR prev t
  pure rt

isMeta ∷ Node w (Value a) → Bool
isMeta n = case rootLabel n of
  Left{}  → True
  Right{} → False


-- * Frozen trees

-- | An unmutable tree with the same "shape" as 'Unsat'.
type Frozen a = Tree (Value a)


-- * Saturated trees

-- | A saturated tree.
newtype Sat a = Sat { runSat ∷ Tree a }

-- | Fill holes in an unsaturated tree with the generator.
saturate
  ∷ ∀ m a
  . MonadST m
  ⇒ (a → m (Sat a))
  → Unsat (World m) a
  → m (Sat a)
saturate gen = join . cataR step
  where
  step ∷ Value a → [m (Sat a)] → m (Sat a)
  step v xs = case v of
    Left a  → gen a
    Right a → Sat . Tree.Node a . map runSat <$> sequence xs
