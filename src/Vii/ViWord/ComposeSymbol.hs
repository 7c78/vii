{-# LANGUAGE MultiWayIf #-}
module Vii.ViWord.ComposeSymbol where

import Control.Applicative
import Data.Text qualified as T
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Functor
import Vii.ViSymbol
import Vii.ViWord.Component
import Vii.ViWord.Type

data CompositionError
    = InvalidComposition
    | DuplicateConsonantMark ConsonantMarkSymbol
    | DuplicateVowelMark VowelMarkSymbol
    | DuplicateToneMark ToneMarkSymbol
    deriving (Show)

instance Alternative (Either CompositionError) where
    empty = Left InvalidComposition

    Left InvalidComposition <|> r           = r
    Left _                  <|> r@(Right _) = r
    l                       <|> _           = l

withCase :: IsUpper -> ViWord -> ViWord
withCase u vi@ViWord{ uppercaseFlags } =
    vi{ uppercaseFlags = uppercaseFlags <> [u]
      }

-- | composeSymbol
composeSymbol :: ViWord -> ViSymbol -> Either CompositionError ViWord
composeSymbol vi (Consonant (c, u)) = vi `composeConsonant` c <&> withCase u
composeSymbol vi (Vowel (c, u))     = vi `composeVowel` c <&> withCase u
composeSymbol vi (ConsonantMark c)  = vi `composeConsonantMark` c
composeSymbol vi (VowelMark c)      = vi `composeVowelMark` c
composeSymbol vi (ToneMark c)       = vi `composeToneMark` c

composeConsonant :: ViWord -> Char -> Either CompositionError ViWord
composeConsonant vi@ViWord{ c1, w, v, g_c2 } c
    | v == "" =
        if | (c1 `T.snoc` c) `HashSet.member` initialConsonants ->
                Right vi{ c1 = c1 `T.snoc` c
                        }
           | otherwise ->
                Left InvalidComposition

    | otherwise =
        if | (g_c2 `T.snoc` c) `HashSet.member` finalConsonants ->
            if | g_c2 == "" ->
                if | v == "ua" ->
                        Right vi{ w = "u"
                                , v = "a"
                                , g_c2 = T.singleton c
                                }
                   | c1 `elem` ["h", "th"]
                     && w == "u"
                     && v == "o" ->
                        Right vi{ w = ""
                                , v = "uo"
                                , g_c2 = T.singleton c
                                }
                   | otherwise ->
                        Right vi{ g_c2 = T.singleton c
                                }
               | otherwise ->
                    Right vi{ g_c2 = g_c2 `T.snoc` c
                            }
           | otherwise ->
                Left InvalidComposition

composeVowel :: ViWord -> Char -> Either CompositionError ViWord
composeVowel vi@ViWord{ c1, w, v, g_c2 } c
    | g_c2 /= "" =
        Left InvalidComposition

    | v == "" =
        if | c1 == "q" && c == 'u' ->
                Right vi{ c1 = "qu"
                        }
           | otherwise ->
                Right vi{ v = T.singleton c
                        }

    | otherwise =
        if | c1 `elem` ["h", "th"]
             && w == "u"
             && v == "o"
             && c `elem` ['u', 'i'] ->
                Right vi{ w = ""
                        , v = "uo"
                        , g_c2 = T.singleton c
                        }
           | c1 == "g" && v == "i" ->
                if | c `elem` ['i', 'y'] ->
                        Left InvalidComposition
                   | c == 'e' ->
                        Right vi{ v = "ie"
                                }
                   | otherwise ->
                        Right vi{ c1 = "gi"
                                , v = T.singleton c
                                }
           | w == ""
             && or [ v == "o" && c `elem` ['a', 'e']
                   , v == "u" && c `elem` ['e', 'y']
                   , v == "u" && c == 'o' && c1 `elem` ["h", "th"]
                   ] ->
                Right vi{ w = v
                        , v = T.singleton c
                        }
           | or [ v `elem` ["a", "e"]                             && c == 'o'
                , v `elem` ["a", "e", "i", "ie", "ye", "u", "uo"] && c == 'u'
                , v `elem` ["a", "o", "u", "uo"]                  && c == 'i'
                , v `elem` ["a"]                                  && c == 'y'
                ] ->
                Right vi{ g_c2 = T.singleton c
                        }
           | (v `T.snoc` c) `HashSet.member` vowels ->
                Right vi{ v = v `T.snoc` c
                        }
           | otherwise ->
                Left InvalidComposition

composeConsonantMark :: ViWord -> ConsonantMarkSymbol -> Either CompositionError ViWord
composeConsonantMark vi@ViWord{ c1, c1m } c
    | c1m == Just c =
        Left (DuplicateConsonantMark c)
    | (c1, c) `HashMap.member` markedConsonant =
        Right vi{ c1m = Just c }
    | otherwise =
        Left InvalidComposition

composeVowelMark :: ViWord -> VowelMarkSymbol -> Either CompositionError ViWord
composeVowelMark vi@ViWord{ v, vm } c
    | vm == Just c =
        Left (DuplicateVowelMark c)
    | (v, c) `HashMap.member` markedVowels =
        Right vi{ vm = Just c }
    | v == "ua" && c == CircumflexA =
        Right vi{ w = "u"
                , v = "a"
                , vm = Just c
                }
    | otherwise =
        Left InvalidComposition

composeToneMark :: ViWord -> ToneMarkSymbol -> Either CompositionError ViWord
composeToneMark vi@ViWord{ v, t } c
    | v == ""   = Left InvalidComposition
    | t == c    = Left (DuplicateToneMark c)
    | otherwise = Right vi{ t = c }
