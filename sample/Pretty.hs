{-# OPTIONS_GHC -Wall -Wno-orphans #-}
module Pretty (($$), prettyVsep, Pretty(..), Doc) where

import Data.Text.Prettyprint.Doc (Pretty(pretty), Doc)
import qualified Data.Text.Prettyprint.Doc as Doc

($$) ∷ Doc ann → Doc ann → Doc ann
a $$ b = Doc.sep [a, b]

prettyVsep ∷ Pretty a ⇒ [a] → Doc ann
prettyVsep = Doc.sep . map pretty
