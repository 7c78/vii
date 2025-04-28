module IBusVii.PreeditText where

import Prelude hiding (length)
import Data.Text (Text)
import Data.Text qualified as T
import Vii.ViWord as ViWord
import Vii.ViSymbol

type PreeditText = (ViWord, Text)

empty :: PreeditText
empty = (ViWord.empty, "")

null :: PreeditText -> Bool
null (vi, err) = ViWord.null vi && T.null err

length :: PreeditText -> Int
length (vi, err) = ViWord.length vi + T.length err

composeChar :: Char -> [ViSymbol] -> PreeditText -> PreeditText
composeChar char symbols (vi, "") =
    case vi `ViWord.compose` symbols of
        Left InvalidComposition         -> (vi, T.singleton char)
        Left (DuplicateConsonantMark _) -> (vi{c1m=Nothing}, T.singleton char)
        Left (DuplicateVowelMark _)     -> (vi{vm=Nothing}, T.singleton char)
        Left (DuplicateToneMark _)      -> (vi{t=Level}, T.singleton char)
        Right vi'                       -> (vi', "")
composeChar char _ (vi, err) =
    (vi, err `T.snoc` char)

render :: PreeditText -> Text
render (vi, err) = T.concat [ViWord.render vi, err]

removeLastLetter :: PreeditText -> PreeditText
removeLastLetter (vi, err)
    | not (T.null err) = (vi, T.init err)
    | otherwise        = (ViWord.removeLastLetter vi, err)
