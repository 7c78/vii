module Vii.ViWord.Type where

import Prelude hiding (length)
import Data.Text (Text)
import Data.Text qualified as T
import Vii.ViSymbol

data ViWord = ViWord
    { c1   :: Text
    , w    :: Text
    , v    :: Text
    , g_c2 :: Text
    , t    :: ToneMarkSymbol
    , c1m  :: Maybe ConsonantMarkSymbol
    , vm   :: Maybe VowelMarkSymbol
    , uppercaseFlags :: [Bool]
    }
    deriving (Show)

empty :: ViWord
empty = ViWord
    { c1   = ""
    , w    = ""
    , v    = ""
    , g_c2 = ""
    , t    = Level
    , c1m  = Nothing
    , vm   = Nothing
    , uppercaseFlags = []
    }

length :: ViWord -> Int
length ViWord{c1, w, v, g_c2} =
    T.length c1 + T.length w + T.length v + T.length g_c2

null :: ViWord -> Bool
null = (== 0) . length
