{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Disorder.Core.Main

import           System.IO (IO)

import qualified Test.Set1 as Set1
import qualified Test.Set2 as Set2


main :: IO ()
main = disorderMain [
    Set1.tests
  , Set2.tests
  ]
