module Vii.InputMethod where

import Data.HashSet qualified as HashSet
import Data.HashSet (HashSet)
import Data.Char (toUpper, toLower)
import Vii.ViSymbol

type InputMethod = Char -> [ViSymbol]

telex :: InputMethod
telex = \case
    'd' -> [ConsonantMark BarD]
    'D' -> [ConsonantMark BarD]
    'a' -> [VowelMark CircumflexA]
    'A' -> [VowelMark CircumflexA]
    'e' -> [VowelMark CircumflexE]
    'E' -> [VowelMark CircumflexE]
    'o' -> [VowelMark CircumflexO]
    'O' -> [VowelMark CircumflexO]
    'w' -> [VowelMark BreveA, VowelMark HornUO]
    'z' -> [ToneMark Level]
    'f' -> [ToneMark Grave]
    's' -> [ToneMark Acute]
    'r' -> [ToneMark Hook]
    'x' -> [ToneMark Tilde]
    'j' -> [ToneMark Dot]
    _   -> []

vni :: InputMethod
vni = \case
    '0' -> [ToneMark Level]
    '1' -> [ToneMark Acute]
    '2' -> [ToneMark Grave]
    '3' -> [ToneMark Hook]
    '4' -> [ToneMark Tilde]
    '5' -> [ToneMark Dot]
    '6' -> [VowelMark CircumflexA, VowelMark CircumflexE, VowelMark CircumflexO]
    '7' -> [VowelMark HornUO]
    '8' -> [VowelMark BreveA]
    '9' -> [ConsonantMark BarD]
    _   -> []

lowerConsonantSymbols :: HashSet Char
lowerConsonantSymbols = HashSet.fromList
    [ 'b', 'c', 'd', 'g', 'h', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'x' ]

upperConsonantSymbols :: HashSet Char
upperConsonantSymbols = HashSet.map toUpper lowerConsonantSymbols

lowerVowelSymbols :: HashSet Char
lowerVowelSymbols = HashSet.fromList
    [ 'a', 'e', 'i', 'o', 'u', 'y' ]

upperVowelSymbols :: HashSet Char
upperVowelSymbols = HashSet.map toUpper lowerVowelSymbols

readLetter :: Char -> [ViSymbol]
readLetter c
    | c `HashSet.member` lowerVowelSymbols     = [Vowel (c, False)]
    | c `HashSet.member` lowerConsonantSymbols = [Consonant (c, False)]
    | c `HashSet.member` upperConsonantSymbols = [Consonant (toLower c, True)]
    | c `HashSet.member` upperVowelSymbols     = [Vowel (toLower c, True)]
    | otherwise                                = []

readSymbol :: InputMethod -> Char -> Maybe [ViSymbol]
readSymbol im c
    | null symbol = Nothing
    | otherwise   = Just symbol
    where symbol = readLetter c <> im c
