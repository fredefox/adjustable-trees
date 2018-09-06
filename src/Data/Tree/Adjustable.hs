-- | Implementation of mutable trees.
--
-- This module contains a lot of slightly different traversals.  I
-- don't think they all do what they say on the tin.  Stay with
-- 'forEachR' for now.
{-# Language StrictData, Strict #-}
{-# OPTIONS_GHC -Wall #-}
module Data.Tree.Adjustable
  ( R, Node(..)
  -- * Modify nodes.
  , writeR, readR, cataR
  -- * Conversion
  , fromTree, toTree
  -- * Traversals
  , forEachR
  , findDescendents
  -- * Not really checked:
  , foldMapR
  , foldMapNode
  , traverseR
  , traverseNode
  ) where

import           Prelude hiding (lookup, filter)

import           Data.STRef (STRef)
import           Data.Tree (Tree)
import qualified Data.Tree as Tree
import           Data.Semigroup (sconcat)
import           GHC.Base (NonEmpty((:|)))

import           Control.Monad.ST.Class
import           Control.Monad.ST.Lifted
import           Control.Monad ((>=>))
import           Control.Applicative (Alternative(..))

-- | A mutable tree.
newtype R s a = R { runR ∷ STRef s (Node s a) }

-- | Nodes in a mutable tree.
data Node s a = Node
  { rootLabel ∷ a
  , subForest ∷ [R s a]
  }

-- | Read the value inside an 'R'.
readR ∷ MonadST m ⇒ R (World m) a → m (Node (World m) a)
readR = readSTRef . runR

-- | Update the value inside an 'R'.
writeR ∷ MonadST m ⇒ Node (World m) a -> R (World m) a -> m ()
writeR a (R r) = writeSTRef r a

-- | Create a mutable tree from a 'Tree'.
fromTree ∷ ∀ m a . MonadST m ⇒ Tree a → m (R (World m) a)
fromTree = Tree.foldTree (\x xs → sequence xs >>= fmap R . newSTRef . Node x)

-- | \"Freezes\" a mutable tree.
toTree ∷ ∀ m a . MonadST m ⇒ R (World m) a → m (Tree a)
toTree = cataR Tree.Node

-- | Modify all nodes in a DFS manner.  The recursion is performed on
-- the modified node.  Results are accumulated accoring to a
-- 'Semigroup'.
forEachR
  ∷ ∀ m w a
  . MonadST m
  ⇒ Semigroup w
  ⇒ (R (World m) a → m w)
  → R (World m) a
  → m w
forEachR act r = do
  -- Modify current node.
  w ← act r
  -- Read the new value.
  a ← readR r
  -- Recurse.
  ws ← step a
  -- Combine the results.
  pure $ sconcat $ w :| ws
  where
  step ∷ Node (World m) a → m [w]
  step (Node _ xs) = traverse (forEachR act) xs

-- | Find pointers all nodes matching the predicate.
findDescendents
  ∷ ∀ m a
  . MonadST m
  ⇒ (a → Bool)
  → R (World m) a
  → m [R (World m) a]
findDescendents p r = forEachR go r
  where
  go ∷ R (World m) a → m [R (World m) a]
  go r0 = p' <$> readR r0
  p' ∷ Node (World m) a → [R (World m) a]
  p' v = if p (rootLabel v) then pure r else empty

-- | Kinda like 'foldMap' for 'R', except it's in a 'MonadST'.  This
-- means in particular that it's possible to modify the nodes in 'R'
-- before(or after?) recursing.
foldMapR
  ∷ ∀ m w a
  . MonadST m
  ⇒ Monoid w
  ⇒ (R (World m) a → m w)
  -- ^ It's possible to modify the value here!
  → R (World m) a
  → m w
foldMapR f r = readR r >>= \x → foldMapNode go x
  where
  go ∷ Node (World m) a → m w
  go n = newSTRef n >>= f . R

-- | Kinda like 'foldMap' for 'Node', except it's in a 'MonadST'.
foldMapNode
  ∷ ∀ m w a
  . MonadST m
  ⇒ Monoid w
  ⇒ (Node (World m) a → m w)
  → Node (World m) a
  → m w
foldMapNode f t@(Node _ xs)
    =   mappend
    <$> f t
    <*> (mconcat <$> traverse (foldMapR (readR >=> f)) xs)

-- | Like 'traverse' for 'R' except that we need a 'MonadST' rather
-- than just an applicative.
traverseR ∷ MonadST m ⇒ (a → m b) → R (World m) a → m (R (World m) b)
traverseR f r = readR r >>= traverseNode f >>= fmap R . newSTRef

-- | Like 'traverse' for 'Node' except that we need a 'MonadST' rather
-- than just an applicative.
traverseNode
  ∷ ∀ m a b
  . MonadST m
  ⇒ (a → m b)
  → Node (World m) a
  → m (Node (World m) b)
traverseNode f (Node a as)
  =   Node
  <$> f a
  <*> traverse @[] @m (traverseR f) as

-- | A catamorphism for 'R' modulo the fact that the result is in a
-- 'MonadST'.
cataR
  ∷ ∀ m a b
  . MonadST m
  ⇒ (a → [b] → b)
  → R (World m) a
  → m b
cataR f r = readR r >>= cataNode f

-- | A catamorphism for 'Node' modulo the fact that the result is in a
-- 'MonadST'.
cataNode ∷ ∀ m a b . MonadST m ⇒ (a → [b] → b) → Node (World m) a → m b
cataNode f (Node a as) = f a <$> traverse (cataR f) as
