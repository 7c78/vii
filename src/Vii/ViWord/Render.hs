module Vii.ViWord.Render where

import Data.Text (Text)
import Data.Text qualified as T
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Char (toUpper)
import Vii.ViSymbol
import Vii.ViWord.Type
import Vii.ViWord.Component

render :: ViWord -> Text
render (ViWord c1 w v g_c2 t c1m vm uppercaseFlags) =
    let c1' = renderInitialCluster c1 c1m
        v' = renderVowel v vm t
        word = T.unpack $ T.concat [c1', w, v', g_c2]
        word' = zip word uppercaseFlags
     in T.pack $ map renderLetter word'

renderInitialCluster :: Text -> Maybe ConsonantMarkSymbol -> Text
renderInitialCluster c1 Nothing  = c1
renderInitialCluster c1 (Just m) = fromMaybe c1 $ HashMap.lookup (c1, m) markedConsonant

renderVowel :: Text -> Maybe VowelMarkSymbol -> ToneMarkSymbol -> Text
renderVowel v Nothing  t = renderVowelTone v t
renderVowel v (Just m) t = renderVowelTone (renderVowelMark v m) t

renderVowelTone :: Text -> ToneMarkSymbol -> Text
renderVowelTone v Level = v
renderVowelTone v t     = fromMaybe v $ HashMap.lookup (v, t) tonedVowels

renderVowelMark :: Text -> VowelMarkSymbol -> Text
renderVowelMark v m = fromMaybe v $ HashMap.lookup (v, m) markedVowels

renderLetter :: (Char, IsUpper) -> Char
renderLetter (c, True) = toUpper c
renderLetter (c, _)    = c
