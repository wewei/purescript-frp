module Test.Reactive.Guarantee where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactive.Guarantee (makeGuarantee)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


testGuarantee :: Effect Unit
testGuarantee = launchAff_ $ runSpec [consoleReporter] do
    describe "Guarantee" do
        describe "makeGuarantee" do
            it "should create a Guarantee object correctly" do
                pure unit
