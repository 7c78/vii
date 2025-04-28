-- https://github.com/ibus/ibus/blob/main/ibus/text.py
module IBus.Text where

import Data.Word (Word32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DBus.Internal.Types (IsVariant(..), Variant, Structure(..), toValue)
import IBus.Attribute

data IBusText = IBusText
    Text
    IBusAttrList

new :: Text -> IBusText
new s = IBusText s (IBusAttrList [])

attribute :: Word32 -> Word32 -> Word32 -> Word32 -> IBusText -> IBusText
attribute typ val start end (IBusText text (IBusAttrList attrs)) =
    let attr = IBusAttribute typ val start end
        attrList = IBusAttrList (attrs <> [attr])
     in IBusText text attrList

instance IsVariant IBusText where
    toVariant (IBusText text attrs) =
        toVariant $ Structure
            [ toValue ("IBusText" :: Text)
            , toValue (Map.empty :: Map Text Variant)
            , toValue text
            , toValue (toVariant attrs)
            ]

    fromVariant =
        undefined
