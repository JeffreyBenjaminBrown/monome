module Util.Byte (
  ByteString
  , pack
  , unpack
  , fi
  , numBetween
  )

where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

-- | Because OSC needs a lot of Int32 values while I prefer Int.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

numBetween :: Real a => a -> a -> a -> Bool
numBetween x low high = x >= low && x <= high
