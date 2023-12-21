module Test.Main where

import Prelude

import Effect (Effect)
import Test.Reactive.Guarantee (testGuarantee)

main :: Effect Unit
main = do
  testGuarantee
