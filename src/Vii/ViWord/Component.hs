module Vii.ViWord.Component where

import Data.Function
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T
import Vii.ViSymbol

initialConsonants :: HashSet Text
initialConsonants = HashSet.fromList $ map removeDiacritics
    [ "b", "c", "d", "đ", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x"
    , "ch", "gh", "gi", "kh", "ng", "nh", "ph", "th", "tr"
    , "ngh"
    ]

finalConsonants :: HashSet Text
finalConsonants = HashSet.fromList
    [ "c", "ch", "m", "n", "ng", "nh", "p", "t" ]

vowels :: HashSet Text
vowels = HashSet.fromList $ map removeDiacritics
    [ "a", "ă", "â", "e", "ê", "i", "o", "ô", "ơ", "u", "ư", "y"
    -- iê
    , "ia", "iê", "ya", "yê"
    -- ươ
    , "ưa", "ươ"
    -- uô
    , "ua", "uô"
    ]

markedConsonant :: HashMap (Text, ConsonantMarkSymbol) Text
markedConsonant = HashMap.singleton ("d", BarD) "đ"

markedVowels :: HashMap (Text, VowelMarkSymbol) Text
markedVowels = HashMap.fromList
    [ "a"  & mark BreveA      "ă"

    , "a"  & mark CircumflexA "â"

    , "e"  & mark CircumflexE "ê"
    , "ie" & mark CircumflexE "iê"
    , "ye" & mark CircumflexE "yê"

    , "o"  & mark CircumflexO "ô"
    , "uo" & mark CircumflexO "uô"

    , "o"  & mark HornUO      "ơ"

    , "u"  & mark HornUO      "ư"
    , "ua" & mark HornUO      "ưa"
    , "uo" & mark HornUO      "ươ"
    ]
    where mark c to from = ((from, c), to)

tonedVowels1 :: HashMap (Text, ToneMarkSymbol) Text
tonedVowels1 = HashMap.fromList $ concat
    [ "a" & tone "à" "á" "ả" "ã" "ạ"
    , "ă" & tone "ằ" "ắ" "ẳ" "ẵ" "ặ"
    , "â" & tone "ầ" "ấ" "ẩ" "ẫ" "ậ"
    , "e" & tone "è" "é" "ẻ" "ẽ" "ẹ"
    , "ê" & tone "ề" "ế" "ể" "ễ" "ệ"
    , "i" & tone "ì" "í" "ỉ" "ĩ" "ị"
    , "o" & tone "ò" "ó" "ỏ" "õ" "ọ"
    , "ô" & tone "ồ" "ố" "ổ" "ỗ" "ộ"
    , "ơ" & tone "ờ" "ớ" "ở" "ỡ" "ợ"
    , "u" & tone "ù" "ú" "ủ" "ũ" "ụ"
    , "ư" & tone "ừ" "ứ" "ử" "ữ" "ự"
    , "y" & tone "ỳ" "ý" "ỷ" "ỹ" "ỵ"
    ]
    where
        tone grave acute hook tilde dot level =
            [ ((level, Grave), grave)
            , ((level, Acute), acute)
            , ((level, Hook),  hook)
            , ((level, Tilde), tilde)
            , ((level, Dot),   dot)
            ]

tonedVowels2 :: HashMap (Text, ToneMarkSymbol) Text
tonedVowels2 = HashMap.fromList $ concat
    [ "ia" & toneAt 0
    , "ya" & toneAt 0
    , "ua" & toneAt 0
    , "ưa" & toneAt 0

    , "iê" & toneAt 1
    , "yê" & toneAt 1
    , "uô" & toneAt 1
    , "ươ" & toneAt 1
    ]
    where
        toneAt i xs =
            HashMap.foldrWithKey (step s c c') [] tonedVowels1
            where s x | i == 0    = x `T.snoc` (xs `T.index` 1)
                      | otherwise = (xs `T.index` 0) `T.cons` x
                  c' = T.singleton $ xs `T.index` i
                  c = removeDiacritics c'

        step s c c' (from, symbol) to z
            | c == from  = ((s c, symbol), s to) : z
            | c' == from = ((s c', symbol), s to) : z
            | otherwise  = z

tonedVowels :: HashMap (Text, ToneMarkSymbol) Text
tonedVowels = HashMap.union tonedVowels1 tonedVowels2
