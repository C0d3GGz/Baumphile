module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Console as Console
import Record as Record

data Box a = BoxC a
data Phantom a = PhantomC
data Option a = Some a | None
data Seq a = ConsS a (Seq a) | NilS
data Cont r a = ContC ((a -> r) -> r)
data Parser a = ParserC (String -> Option { value :: a, remaining :: String })

parseString :: String -> Parser String
parseString = \str -> ParserC \input -> 
 case String.stripPrefix (String.Pattern str) input of
   Nothing -> None
   Just remaining -> Some { value : str, remaining }

runParser :: forall a. Parser a -> String -> Option a
runParser = \parser input -> case parser of
  ParserC parseF -> mapF _.value (parseF input)

unParser ::  forall a. Parser a -> String -> Option { value :: a, remaining :: String }
unParser = \ (ParserC parser) -> parser

mapParser :: forall a b. (a -> b) -> Parser a -> Parser b
mapParser = \f parser -> case parser of
  ParserC parseF -> ParserC \input -> case parseF input of
    Some record -> Some { value : f record.value, remaining : record.remaining }
    None -> None

-- mapParser :: forall a b. (a -> b) -> Parser a -> Parser b
-- mapParser = \f parser -> case parser of
--   ParserC parseF -> ParserC (mapF (Record.modify (SProxy :: _ "value") f) <<< parseF)

applüParser :: forall a b. Parser (a -> b) -> Parser a -> Parser b
applüParser = case _,_ of
  ParserC parseF, ParserC parseA -> ParserC \input -> case parseF input of
    None -> None
    Some { value : f, remaining } -> case parseA remaining of
      None -> None
      Some { value : a, remaining : remaining2 } -> Some { value : f a, remaining: remaining2 }

püreParser :: forall a. a -> Parser a
püreParser = \value -> ParserC \remaining -> Some { value, remaining }

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

instance funktorParser :: Funktor Parser where
  mapF = mapParser

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

instance appParser :: Applikative Parser where
  applü = applüParser
  püre = püreParser

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


class Applikative f <= Mönad f where
  flätMäp :: forall a b. f a -> (a -> f b) -> f b
infixl 1 flätMäp as >>

flätMäpBox :: forall a b. Box a -> (a -> Box b) -> Box b
flätMäpBox = \ (BoxC a) f -> f a

flätMäpPhantom :: forall a b. Phantom a -> (a -> Phantom b) -> Phantom b
flätMäpPhantom = \ _ _ -> PhantomC

flätMäpOption  :: forall a b. Option a -> (a -> Option b) -> Option b
flätMäpOption = \option f -> case option of 
  None -> None
  Some x -> f x

flätMäpSeq  :: forall a b. Seq a -> (a -> Seq b) -> Seq b
flätMäpSeq = \seq f -> case seq of
  NilS -> NilS
  ConsS x xs -> concat (f x) (flätMäpSeq xs f)

{- flätMäpCont  :: forall a b r. Cont r a -> (a -> Cont r b) -> Cont r b
flätMäpCont = \ (ContC ka) f -> ContC \kb -> ka \a -> case (f a) of 
  ContC kb' -> kb' kb -}

flätMäpCont  :: forall a b r. Cont r a -> (a -> Cont r b) -> Cont r b
flätMäpCont = \ (ContC ka) f -> ContC \kb -> ka \a -> runCont (f a) kb

flätMäpParser :: forall a b. Parser a -> (a -> Parser b) -> Parser b
flätMäpParser = \ (ParserC parseA) f -> ParserC \input -> case parseA input of
  None -> None
  Some { value, remaining } -> unParser (f value) remaining

instance fmb :: Mönad Box where
  flätMäp = flätMäpBox

instance fmP :: Mönad Phantom where
  flätMäp = flätMäpPhantom

instance fmS :: Mönad Seq where
  flätMäp = flätMäpSeq

instance fmO :: Mönad Option where
  flätMäp = flätMäpOption

instance fmC :: Mönad (Cont r) where
  flätMäp = flätMäpCont

instance fmParser :: Mönad Parser where
  flätMäp = flätMäpParser


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

  -- async await
  let a = ContC (\k -> k 42)
  let b = \x -> if x > 10 then ContC (\k -> k "large") else ContC (\k -> k "small")
  let largeOrSmall = flätMäpCont a b
  runCont largeOrSmall Console.log 



  let parserHello = parseString "Hallo"
  let parserWorld = parseString "Welt"
  let input = "HalloWelt"
  let combine = \str str2 -> str <> " " <> str2 <> "!"

  let input2 = "Int: 42" -- should use int parser
  let input3 = "String: abc" -- should use string parser

  --let parserHelloWorld = applüParser (mapParser combine parserHello) parserWorld
  -- let parserHelloWorld = liftZwei combine parserHello parserWorld

  let
    parserHelloWorld = 
      parserHello >> \hello -> 
        parserWorld >> \world ->
          püre (combine hello world)

{-   do
    hello <- parserHello
    world <- parserWorld
    püre (combine hello world) -}

  case runParser parserHelloWorld input of
    None -> Console.log "parsing failed"
    Some res -> Console.log ("sucessfully parsed: " <> res)
  pure unit