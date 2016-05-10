{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Set2 where


import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Word

import           P

import qualified Prelude

import           Set1


data CBCMode = Encrypt | Decrypt

cbcEncrypt_AES128 :: ByteString -> ByteString -> ByteString -> Maybe ByteString
cbcEncrypt_AES128 iv key text = case blocks of
  [] -> pure mempty
  (x:xs) -> finalise <$> (first x >>= \y -> foldl' fun (Just (y, mempty)) xs)
  where
    cipher = ecbEncrypt_AES128 key
    blocks = bsBlocks 16 (pkcs7Blocks 16 text)
    first x = fixedXor iv x >>= cipher
    fun Nothing _ = Nothing
    fun (Just (last, rest)) b =
      let xord = fixedXor last b
      in xord >>= cipher >>= \n -> pure (n, rest <> BB.byteString last)
    finalise (last, rest) = BL.toStrict . BB.toLazyByteString $ rest <> BB.byteString last

cbcDecrypt_AES128 :: ByteString -> ByteString -> ByteString -> Maybe ByteString
cbcDecrypt_AES128 iv key text = case blocks of
  [] -> pure mempty
  (x:xs) -> finalise <$> (first x >>= \y -> foldl' fun (Just (x, BB.byteString y)) xs)
  where
    cipher = ecbDecrypt_AES128 key
    blocks = bsBlocks 16 text
    first x = cipher x >>= fixedXor iv
    fun Nothing _ = Nothing
    fun (Just (last, rest)) b =
      let xord = fixedXor last
      in cipher b >>= xord >>= \n -> pure (b, rest <> BB.byteString n)
    finalise (last, rest) = BL.toStrict . BB.toLazyByteString $ rest



-- | the method shall be to pad the input at the trailing end with
-- k - (l mod k) octets all having value k - (l mod k), where
-- l is the length of the input.
-- FIX probably wrong - needs to add _at least one_ byte of padding to be reversible
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
