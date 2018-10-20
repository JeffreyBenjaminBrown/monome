module Util.Byte (
  ByteString
  , pack
  , unpack
  , fi
  )

where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

-- | Because OSC needs a lot of Int32 values while I prefer Int.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
