{-# LANGUAGE MultiWayIf #-}
module Vii.ViWord.RemoveLastLetter where

import Data.Function
import Data.Text qualified as T
import Data.Maybe
import Vii.ViSymbol
import Vii.ViWord.Type

removeLastLetter :: ViWord -> ViWord
removeLastLetter vi@ViWord{ c1, v, g_c2 }
    | g_c2 /= "" = removeFromFinalCluster vi & removeUppercaseFlag
    | v /= ""    = removeFromVowel vi & removeUppercaseFlag
    | c1 /= ""   = removeFromInitialCluster vi & removeUppercaseFlag
    | otherwise  = vi

removeUppercaseFlag :: ViWord -> ViWord
removeUppercaseFlag vi@ViWord{ uppercaseFlags } =
    vi{ uppercaseFlags = init uppercaseFlags
      }

removeFromFinalCluster :: ViWord -> ViWord
removeFromFinalCluster vi@ViWord{c1, w, v, g_c2, vm}
    | T.length g_c2 == 1 =
        if | w == "u"
             && v == "a"
             && isNothing vm ->
                vi{ w = ""
                  , v = "ua"
                  , g_c2 = ""
                  }
           | c1 `elem` ["h", "th"]
             && v == "uo" ->
                vi{ w = "u"
                  , v = "o"
                  , g_c2 = ""
                  }
           | otherwise ->
                vi{ g_c2 = ""
                  }

    | otherwise =
        vi{ g_c2 = T.init g_c2
          }

removeFromVowel :: ViWord -> ViWord
removeFromVowel vi@ViWord{ c1, w, v }
    | T.length v == 2 =
        if | v `T.index` 1 == 'a' ->
                vi{ v = T.init v
                  }
           | otherwise ->
                vi{ v = T.init v
                  , vm = Nothing
                  , t = Level
                  }

    | otherwise = -- T.length v == 1
        if | c1 == "gi" ->
                vi{ c1 = "g"
                  , v = "i"
                  , vm = Nothing
                  , t = Level
                  }
           | w `elem` ["o", "u"] ->
                vi { w = ""
                   , v = w
                   , vm = Nothing
                   , t = Level
                   }
           | otherwise ->
                vi { v = ""
                   , vm = Nothing
                   , t = Level
                   }

removeFromInitialCluster :: ViWord -> ViWord
removeFromInitialCluster vi@ViWord{ c1 } =
    vi{ c1 = T.init c1
      }
