module Reactive.Guarantee where

import Prelude

import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Alt, class Plus)
import Data.Foldable (for_)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (modify_, new, read)

type Handler a = a -> Effect Unit

-- | A Guarantee is a solemn promise, it can be assumed to throw no exception
newtype Guarantee a = Guarantee (Effect (Handler (Handler a)))

runGuarantee :: forall a. Guarantee a -> Effect (Handler (Handler a))
runGuarantee (Guarantee eff) = eff

makeGuarantee :: forall a. Effect (Handler (Handler a)) -> Guarantee a
makeGuarantee eff = Guarantee do
    refHds <- new Nil
    refVal <- new Nothing
    hhd    <- eff
    hhd $ \val -> read refVal >>=
        case _ of
            Just _  -> pure unit
            Nothing -> do
                hds <- read refHds
                modify_ (const (Just val)) refVal
                modify_ (const Nil) refHds
                for_ (reverse hds) (_ $ val)
    pure $ \hdl -> read refVal >>=
        case _ of
            Just v  -> hdl v
            Nothing -> modify_ (hdl:_) refHds

instance Functor Guarantee where
    map f grt = makeGuarantee do
        hhd <- runGuarantee grt
        pure $ \hdl -> hhd (hdl <<< f)

instance Alt Guarantee where
    alt grtX grtY = makeGuarantee do
        hhdX <- runGuarantee grtX
        hhdY <- runGuarantee grtY
        pure $ \hdl -> do
            hhdX hdl
            hhdY hdl

never :: forall a. Guarantee a
never = Guarantee <<< pure <<< const <<< pure $ unit

instance Plus Guarantee where
    empty = never

both :: forall a b. Guarantee a -> Guarantee b -> Guarantee (Tuple a b)
both grtA grtB = makeGuarantee $ do
    refA <- new Nothing
    refB <- new Nothing
    hhdA <- runGuarantee grtA
    hhdB <- runGuarantee grtB
    pure $ \hdl -> do
        hhdA $ \valA -> read refB >>=
            case _ of
                Just valB -> hdl (valA /\ valB)
                Nothing   -> modify_ (const <<< Just $ valA) refA
        hhdB $ \valB -> read refA >>=
            case _ of
                Just valA -> hdl (valA /\ valB)
                Nothing   -> modify_ (const <<< Just $ valB) refB
            
infixr 6 both as <&>

instance Apply Guarantee where
    apply grtF grtA = makeGuarantee do
        hhd <- runGuarantee $ grtF <&> grtA
        pure $ \hdl -> hhd
             $ \(f /\ val) -> hdl (f val)

instance Applicative Guarantee where
    pure val = Guarantee $ pure (_ $ val)

instance Alternative Guarantee

instance Bind Guarantee where
    bind grt f = makeGuarantee do
        hhd <- runGuarantee grt
        pure $ \hdl -> hhd
             $ \val -> runGuarantee (f val) >>= (_ $ hdl)

instance Monad Guarantee

instance MonadPlus Guarantee

instance MonadEffect Guarantee where
    liftEffect eff = Guarantee do
        val <- eff
        pure $ \hdl -> hdl val
