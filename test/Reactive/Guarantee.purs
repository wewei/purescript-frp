module Test.Reactive.Guarantee where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactive.Guarantee (makeGuarantee, runGuarantee)
import Test.Mock (mockEffect, shouldHaveBeenCalledTimes, shouldHaveBeenLastCalledWith)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


testGuarantee :: Effect Unit
testGuarantee = launchAff_ $ runSpec [consoleReporter] do
    describe "Guarantee" do
        describe "makeGuarantee" do
            it "should create a Guarantee object correctly" $ liftEffect do
                mck1  <- mockEffect $ \hdl -> hdl 42

                let grt = makeGuarantee $ pure mck1.callback
                mck1 `shouldHaveBeenCalledTimes` 0

                mck2  <- mockEffect $ const <<< pure $ unit
                hhd   <- runGuarantee $ grt
                hhd $ mck2.callback

                mck1 `shouldHaveBeenCalledTimes` 1
                mck2 `shouldHaveBeenCalledTimes` 1
                mck2 `shouldHaveBeenLastCalledWith` 42