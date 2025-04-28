-- https://github.com/ibus/ibus/blob/main/ibus/attribute.py
module IBus.Attribute where

import Data.Word (Word32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DBus.Internal.Types (IsVariant(..), Variant, Structure(..), toValue)

ibus_attr_type_underline  :: Word32
ibus_attr_type_underline  = 1
ibus_attr_type_foreground :: Word32
ibus_attr_type_foreground = 2
ibus_attr_type_background :: Word32
ibus_attr_type_background = 3

ibus_attr_underline_none   :: Word32
ibus_attr_underline_none   = 0
ibus_attr_underline_single :: Word32
ibus_attr_underline_single = 1
ibus_attr_underline_double :: Word32
ibus_attr_underline_double = 2
ibus_attr_underline_low    :: Word32
ibus_attr_underline_low    = 3
ibus_attr_underline_error  :: Word32
ibus_attr_underline_error  = 4

data IBusAttribute = IBusAttribute
    Word32 -- type
    Word32 -- value
    Word32 -- start_index
    Word32 -- end_index

instance IsVariant IBusAttribute where
    toVariant (IBusAttribute typ val start end) =
        toVariant $ Structure
            [ toValue ("IBusAttribute" :: Text)
            , toValue (Map.empty :: Map Text Variant)
            , toValue typ
            , toValue val
            , toValue start
            , toValue end
            ]

    fromVariant =
        undefined

newtype IBusAttrList = IBusAttrList
    [IBusAttribute]

instance IsVariant IBusAttrList where
    toVariant (IBusAttrList attrs) =
        toVariant $ Structure
            [ toValue ("IBusAttrList" :: Text)
            , toValue (Map.empty :: Map Text Variant)
            , toValue (map toVariant attrs)
            ]

    fromVariant =
        undefined
