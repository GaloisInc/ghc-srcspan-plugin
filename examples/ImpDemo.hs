{-# LANGUAGE OverloadedStrings #-}
{- OPTIONS_GHC -fplugin=ImpPluginExplicit #-}
{- OPTIONS_GHC -fplugin=ImpPluginGeneric #-}
module ImpDemo where

import Imp

sum10 :: Imp ()
sum10 = do
  let n = "n"
  let r = "r"
  n =: 0
  r =: 0
  while (n <? 11) $ do
    r =: r + n
    n =: n + 1
  assert (r =? 54)
