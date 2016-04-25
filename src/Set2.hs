{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Set2 where


import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word

import           P

import qualified Prelude

import           Set1


-- | the method shall be to pad the input at the trailing end with
-- k - (l mod k) octets all having value k - (l mod k), where
-- l is the length of the input.
pkcs7Blocks :: Int -> ByteString -> ByteString
pkcs7Blocks k bs = B.concat $ fmap (pkcs7Block k) (bsBlocks k bs)

pkcs7Block :: Int -> ByteString -> ByteString
pkcs7Block k bs
  | B.length bs == k = bs
  | B.length bs < k =
      let l = B.length bs
          b = padByte k l
          p = B.replicate (k - l) b
      in bs <> p

  | otherwise = Prelude.error "pkcs7Block: block size invariant failed"
  where
    padByte :: Int -> Int -> Word8
    padByte k l = fromIntegral $ k - (l `mod` k)
