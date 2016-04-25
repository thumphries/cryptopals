{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Test.Set2 where


import qualified Data.ByteString as B

import           Disorder.Core
import           Disorder.Core.IO

import           P

import           Set1
import           Set2

import           System.IO (readFile)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_pkcs7_unit = once $ pkcs7Blocks 20 inp === exp
  where
    inp = "YELLOW SUBMARINE"
    exp = "YELLOW SUBMARINE\EOT\EOT\EOT\EOT"

prop_pkcs7_length bs k =
  B.length (pkcs7Blocks (getPositive k) bs) `mod` (getPositive k) === 0

prop_pkcs7_noop b k i =
  pkcs7Blocks (getPositive k) inp === inp
  where
    inp = B.replicate (getPositive i * getPositive k) b


return []
tests = $quickCheckAll
