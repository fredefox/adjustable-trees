{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# Language OverloadedStrings #-}
module Pretty.Orphans where

import Data.Tree (Tree)
import qualified Data.Tree as Tree

import qualified Data.Text.Prettyprint.Doc as Doc
import Pretty

instance Pretty a ⇒ Pretty (Tree a) where
  pretty (Tree.Node x xs) = case xs of
    [] → pretty x
    _  → "(" <> pretty x <> Doc.nest 2 (mempty $$ prettyVsep xs) <> ")"

instance (Show a, Show b) ⇒ Pretty (Either a b) where
  pretty = pretty . show
