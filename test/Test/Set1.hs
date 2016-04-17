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


return []
tests = $quickCheckAll
