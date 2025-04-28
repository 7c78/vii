module IBusVii.Word32 where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Char (chr)
import Data.Word (Word32)

chr32 :: Word32 -> Char
chr32 = chr . fromIntegral

textLength :: Text -> Word32
textLength = fromIntegral . T.length
