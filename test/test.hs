{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Disorder.Core.Main

import           System.IO (IO)

import qualified Test.Set1 as Set1


main :: IO ()
main = disorderMain [
    Set1.tests
  ]
