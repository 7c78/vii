-- https://github.com/ibus/ibus/blob/main/ibus/modifier.py
module IBus.Modifier where

import Data.Word (Word32)
import Data.Bits (shiftL)

shift_mask :: Word32
shift_mask = 1 `shiftL` 0

lock_mask :: Word32
lock_mask = 1 `shiftL` 1

control_mask :: Word32
control_mask = 1 `shiftL` 2

alt_mask :: Word32
alt_mask = 1 `shiftL` 3

ignored_mask :: Word32
ignored_mask = 1 `shiftL` 25

super_mask :: Word32
super_mask = 1 `shiftL` 26

hyper_mask :: Word32
hyper_mask = 1 `shiftL` 27

meta_mask :: Word32
meta_mask = 1 `shiftL` 28

release_mask :: Word32
release_mask = 1 `shiftL` 30
