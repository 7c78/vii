module Vii.ViSymbol where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

data ViSymbol
    = ToneMark ToneMarkSymbol
    | VowelMark VowelMarkSymbol
    | ConsonantMark ConsonantMarkSymbol
    | Vowel VowelSymbol
    | Consonant ConsonantSymbol
    deriving (Show)

type IsUpper = Bool
type ConsonantSymbol = (Char, IsUpper)
type VowelSymbol = (Char, IsUpper)

data ToneMarkSymbol
    = Level
    | Acute
    | Grave
    | Hook
    | Tilde
    | Dot
    deriving (Show, Eq, Generic, Hashable)

data VowelMarkSymbol
    = BreveA
    | CircumflexA
    | CircumflexE
    | CircumflexO
    | HornUO
    deriving (Show, Eq, Generic, Hashable)

data ConsonantMarkSymbol
    = BarD
    deriving (Show, Eq, Generic, Hashable)

removeDiacritic :: Char -> Char
removeDiacritic c =
    fromMaybe c (HashMap.lookup c accentMap)
    where
        accentMap = HashMap.fromList $ zip "đăâêôơư"
                                           "daaeoou"

removeDiacritics :: Text -> Text
removeDiacritics = T.map removeDiacritic
