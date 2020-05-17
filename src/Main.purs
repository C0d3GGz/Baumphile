module Main (main) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = { label :: Maybe String, currentInput :: String }

data Action = Toggle | InputChanged String

component :: forall q i o m. H.Component HH.HTML q i o m
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
    

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st { label = Just st.currentInput }
  InputChanged newText -> H.modify_ (_ { currentInput = newText }) -- \st -> st <-> (_ {...})
    --H.modify_ \st -> st { enabled = not st.enabled }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body