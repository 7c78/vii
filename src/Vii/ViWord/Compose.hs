module Vii.ViWord.Compose where

import Control.Applicative
import Vii.ViSymbol
import Vii.ViWord.Type
import Vii.ViWord.ComposeSymbol

-- | compose
compose :: ViWord -> [ViSymbol] -> Either CompositionError ViWord
compose vi = asum . map (composeSymbol vi)
