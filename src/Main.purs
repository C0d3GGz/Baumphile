module Main (main) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as RF
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = { label :: Maybe String, currentInput :: String }
type Repo = { name :: String }

data Action = Toggle | InputChanged String

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { label: Nothing, currentInput: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = fromMaybe "Click me!" state.label
  in
    HH.div_ 
      [ HH.input 
        [ HP.placeholder "Type in me..."
        , HP.value state.currentInput
        , HE.onValueInput (Just <<< InputChanged) -- f <<< g = \x -> f (g x)
        ]
      , HH.button
        [ HP.title label
        , HE.onClick \_ -> if String.trim state.currentInput == "" then Nothing else Just Toggle
        ]
        [ HH.text label ]
      ]
    

handleAction ∷ forall o. Action → H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Toggle -> do
    liftAff (AX.get RF.json "https://api.github.com/users/alexdobry/repos") >>= case _ of
      Left err -> 
        Console.log (AX.printError err)
      Right {body} -> case A.decodeJson body of
        Left parseErr -> 
          Console.log parseErr
        Right (repos :: Array Repo) -> 
          for_ repos (Console.log <<< _.name)
            

    H.modify_ \st -> st { label = Just st.currentInput }


  InputChanged newText ->
     Console.log newText >>= \_ ->
       H.modify_ (_ { currentInput = newText }) -- \st -> st <-> (_ {...})
    --H.modify_ \st -> st { enabled = not st.enabled }

main :: Effect Unit
main = HA.runHalogenAff
  (HA.awaitBody >>= runUI component unit)