module Main (main) where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Console as Console

data Box a = BoxC a
data Phantom a = PhantomC
data Option a = Some a | None
data Seq a = ConsS a (Seq a) | NilS
data Cont r a = ContC ((a -> r) -> r)

class Funktor (f :: Type -> Type) where
  mapF :: forall a b. (a -> b) -> f a -> f b

instance funktorb :: Funktor Box where
  mapF = mapBox

instance funktorP :: Funktor Phantom where
  mapF = mapPhantom

instance funcktorO :: Funktor Option where
  mapF = mapOption

instance funktorS :: Funktor Seq where
  mapF = mapSeq

instance funktorC :: Funktor (Cont r) where
  mapF = mapCont

class Funktor f <= Applikative f where
  applü :: forall a b. f (a -> b) -> f a -> f b
  püre :: forall a. a -> f a

instance appb :: Applikative Box where
  applü = applyBox
  püre = BoxC

instance appP :: Applikative Phantom where
  applü = applyPhantom
  püre = \_ -> PhantomC

instance appO :: Applikative Option where
  applü = applyOption
  püre = Some

instance appS :: Applikative Seq where
  applü = applySeq
  püre = \a -> ConsS a NilS

instance appC :: Applikative (Cont r) where
  applü = applyCont
  püre = \a -> ContC (\f -> f a)

defaultMap :: forall f a b. Applikative f => (a -> b) -> f a -> f b
defaultMap = applü <<< püre

mapPhantom :: forall a b. (a -> b) -> Phantom a -> Phantom b
mapPhantom = \_ _ -> PhantomC

mapBox :: forall a b. (a -> b) -> Box a -> Box b
mapBox = \f box -> case box of
  BoxC a -> BoxC (f a)

mapOption :: forall a b. (a -> b) -> Option a -> Option b
mapOption = \f o -> case o of
  Some a -> Some (f a)
  None -> None

mapSeq :: forall a b. (a -> b) -> Seq a -> Seq b
mapSeq = \f seq -> case seq of
  ConsS h t -> ConsS (f h) (mapSeq f t)
  NilS -> NilS

mapCont :: forall a b r. (a -> b) -> Cont r a -> Cont r b
mapCont = \f c -> case c of
  ContC k -> ContC (\kb -> k (kb <<< f)) -- f: a -> b ; kb: b -> r ; k: (a -> r) -> r

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont = case _ of
  ContC k -> k

ignoreF :: forall a f. Funktor f => f a -> f Unit
ignoreF = mapF (const unit)

negateO :: Option Int -> Option Int
negateO = mapF negate

plusO :: Option Int -> Option Int -> Option Int
plusO = \o1 o2 -> liftZwei add o1 o2

applyOption :: forall a b. Option (a -> b) -> Option a -> Option b
applyOption = case _, _ of
  Some f, Some a -> Some (f a)
  _, _ -> None

applyBox :: forall a b. Box (a -> b) -> Box a -> Box b
applyBox = case _, _ of
  BoxC f, BoxC a -> BoxC (f a)

applyPhantom :: forall a b. Phantom (a -> b) -> Phantom a -> Phantom b
applyPhantom = \_ _ -> PhantomC

applyCont :: forall a b r. Cont r (a -> b) -> Cont r a -> Cont r b
applyCont = case _, _ of
  ContC kf, ContC ka -> ContC \kb -> 
    kf \kab -> 
      ka (kb <<< kab)

applySeq :: forall a b. Seq (a -> b) -> Seq a -> Seq b
applySeq = \fs as -> case fs of
  ConsS hf tfs -> concat (mapF hf as) (applySeq tfs as)
  NilS -> NilS

concat :: forall a. Seq a -> Seq a -> Seq a
concat = \s1 s2 -> case s1 of
  ConsS h t -> ConsS h (concat t s2)
  NilS -> s2

type Person = { name :: String, age :: Int }

getName :: forall r. Cont r String
getName = ContC (\f -> f "Döni")

getAge :: forall r. Cont r Int
getAge = ContC (\f -> f 23)

getDöni :: forall r. Cont r Person
getDöni = applyCont (mapF (\name age -> { name, age }) getName) getAge

useDöni :: String
useDöni = runCont getDöni (\p -> p.name)

liftZwei :: forall f a b c. Applikative f => (a -> b -> c) -> f a -> f b -> f c -- zip
liftZwei = \f -> applü <<< mapF f -- \f fa fb -> applü (mapF f fa) fb

main :: Effect Unit
main = do
  pure unit