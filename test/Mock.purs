module Test.Mock where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.List (List(..), length, (:))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref (modify_, new, read)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec.Assertions (fail)

type Invocation a b = { params :: a, result :: b }
type Mock a b =
    { callback    :: a -> b
    , invocations :: Effect (List (Invocation a b))
    , reset       :: Effect Unit
    }

mockImpl :: forall a b. (Effect b -> b) -> (a -> b) -> Effect (Mock a b)
mockImpl exec f = do
    refIvks <- new (Nil :: List (Invocation a b))
    let callback valA =
            exec do let valB = f valA
                    let ivk  = { params: valA, result: valB }
                    modify_ (ivk : _) refIvks
                    pure valB
        invocations   = read refIvks
        reset         = modify_ (const Nil) refIvks
    pure { callback, invocations, reset }

mockEffect :: forall a b. (a -> Effect b) -> Effect (Mock a (Effect b))
mockEffect = mockImpl join

mockPure :: forall a b. (a -> b) -> Effect (Mock a b)
mockPure = mockImpl unsafePerformEffect

shouldHaveBeenCalledTimes :: forall a b m.
                             MonadEffect m => MonadThrow Error m =>
                             Mock a b -> Int -> m Unit
shouldHaveBeenCalledTimes mck n = do
    ivks <- liftEffect mck.invocations
    let m = length ivks
    when (m /= n) $
        fail $ "The function is called " <> show m <> " ≠ " <> show n <> " time(s)"

shouldHaveBeenLastCalledWith :: forall a b m.
                                MonadEffect m => MonadThrow Error m =>
                                Show a => Eq a =>
                                Mock a b -> a -> m Unit
shouldHaveBeenLastCalledWith mck val =
    liftEffect mck.invocations >>= case _ of
        (ivk:_) -> when (ivk.params /= val) $
                    fail $ "The function parameter is " <> show ivk.params <> " ≠ " <> show val 
        _       -> fail "The function is not called"
                                
