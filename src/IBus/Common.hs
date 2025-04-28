-- https://github.com/ibus/ibus/blob/main/ibus/common.py
module IBus.Common where

import Data.Word (Word32)

-- https://github.com/ibus/ibus/blob/d0ad4e6e519439f034d08fd6531403027bd7fbde/src/ibustypes.h#L137
ibus_engine_preedit_clear  :: Word32
ibus_engine_preedit_clear  = 0
ibus_engine_preedit_commit :: Word32
ibus_engine_preedit_commit = 1
