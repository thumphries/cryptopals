{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Set1 where

import           Disorder.Core

import           P

import           Set1

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_fromHex_toBase64_unit = once $
  let str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
      res = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  in liftM toBase64 (fromHex str) === pure res

prop_unBase64Triple_no_fail x y z = unBase64Triple x y z /= ('*', '*', '*', '*')

prop_hexPair_total w = tripping hexPair (uncurry unHexPair)

prop_toHex_fromHex bs = tripping toHex fromHex

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



return []
tests = $quickCheckAll
