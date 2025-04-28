-- key symbols
-- https://github.com/ibus/ibus/blob/main/ibus/keysyms.py
module IBus.Key where

import Data.Word (Word32)


backspace :: Word32
backspace = 0xff08

tab :: Word32
tab = 0xff09

shift_l :: Word32
shift_l = 0xFFE1
shift_r :: Word32
shift_r = 0xFFE2
