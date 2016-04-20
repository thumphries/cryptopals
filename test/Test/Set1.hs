{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Set1 where

import           Data.ByteString as B hiding (readFile)

import           Disorder.Core
import           Disorder.Core.IO

import           P

import qualified Prelude

import           Set1

import           System.IO (IO, readFile)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_fromHex_toBase64_unit = once $
  let str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f\
            \69736f6e6f7573206d757368726f6f6d"
      res = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  in liftM toBase64 (fromHex str) === pure res

prop_unBase64Triple_no_fail x y z = unBase64Triple x y z /= ('*', '*', '*', '*')

prop_hexPair_total w = tripping hexPair (uncurry unHexPair)

prop_toHex_fromHex = tripping toHex fromHex

prop_fixedXor_unit = once $ res === pure expect
  where
    expect = "746865206b696420646f6e277420706c6179"
    res = do
      let str1 = "1c0111001f010100061a024b53535009181c"
          str2 = "686974207468652062756c6c277320657965"
      bs1 <- fromHex str1
      bs2 <- fromHex str2
      bs3 <- fixedXor bs1 bs2
      pure $ toHex bs3

prop_unFixedXor_unit = once $ res === pure expect
  where
    expect = "cOOKING\NULmc\aS\NULLIKE\NULA\NULPOUND\NULOF\NULBACON"
    res = do
      let hex = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      bs <- fromHex hex
      pure . snd $ unSingleByteXor bs

prop_detectSingleByteXor_unit = once . testIO $ do
  res <- detectSingleByteXor . mapMaybe fromHex . Prelude.lines <$> readFile "data/4.txt"
  pure $ res === "Now that the party is jumping\n"

prop_repeatingKeyXor_unit = once $ res === expect
  where
    str = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    key = "ICE"
    res = toHex (repeatingKeyXor key str)
    expect = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272\
             \a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

prop_repeatingKey_trip key bs = repeatingKeyXor key (repeatingKeyXor key bs) === bs

prop_hamming_unit = once $ hammingDistance "this is a test" "wokka wokka!!!" === 37

prop_base64_trip bs = tripping toBase64 fromBase64

return []
tests = $quickCheckAll
