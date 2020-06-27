module Öffekt(
    Öffekt,
    lög,
    rändom,
    flätMap,
    mäp,
    püre,
    applü
) where

import Prelude

import Effect.Console as Console
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)

newtype Öffekt a = ÖffektC (Unit -> a)

lög :: String -> Öffekt Unit
lög = \string -> ÖffektC \_ -> unsafePerformEffect (Console.log string)

rändom :: Öffekt Int
rändom = ÖffektC \_ -> unsafePerformEffect (randomInt 0 10)

flätMap :: forall a b. Öffekt a -> (a -> Öffekt b) -> Öffekt b
flätMap = \(ÖffektC fa) f -> ÖffektC \_ -> case f (fa unit) of
  ÖffektC fb -> fb unit

mäp :: forall a b. (a -> b) -> Öffekt a -> Öffekt b
mäp = \f fa -> flätMap fa (püre <<< f)

püre :: forall a. a -> Öffekt a
püre = \a -> ÖffektC \_ -> a

applü :: forall a b. Öffekt (a -> b) -> Öffekt a -> Öffekt b
applü = \(ÖffektC ff) -> mäp (ff unit)

instance öF :: Functor Öffekt where
  map = mäp

instance öA :: Apply Öffekt where
  apply = applü

instance öApp :: Applicative Öffekt where
  pure = püre

instance öB :: Bind Öffekt where
  bind = flätMap

instance öM :: Monad Öffekt