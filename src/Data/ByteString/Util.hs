module Data.ByteString.Util
    (
      dropUntil
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (drop, length)

dropUntil :: Int -> ByteString -> ByteString
dropUntil n b = BS.drop (BS.length b - n) b
