module Vii.Test
    ( module Vii.InputMethod
    , module ViWord
    , readWord
    , word
    , sentence
    , paragraph
    ) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Vii.InputMethod
import Vii.ViWord as ViWord

readWord :: InputMethod -> Text -> Maybe ViWord
readWord im =
    foldM step ViWord.empty . T.unpack
    where
        step vi c = do
            case readSymbol im c of
                Nothing -> Nothing
                Just s  -> case vi `ViWord.compose` s of
                                Right vi' -> Just vi'
                                _         -> Nothing

word :: InputMethod -> Text -> Text
word method =
    fromMaybe "" . fmap ViWord.render . readWord method

sentence :: InputMethod -> Text -> Text
sentence method =
    T.unwords . map (word method) . T.words

paragraph :: InputMethod -> Text -> Text
paragraph method =
    T.unlines . map (sentence method) . T.lines
