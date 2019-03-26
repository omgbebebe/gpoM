module GpoM.Policy.CSE.Parser.Common (xpBool, xpUByte) where

import Text.XML.HXT.Arrow.Pickle
import Data.Word (Word8)

fromBool :: Bool -> Int
fromBool True = 1
fromBool _ = 0

toBool :: Int -> Bool
toBool 1 = True
toBool _ = False

xpBool :: PU Bool
xpBool = xpWrap (toBool, fromBool) xpickle

xpUByte :: PU Word8
xpUByte = xpPrim

