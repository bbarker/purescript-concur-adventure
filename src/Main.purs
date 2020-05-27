module Main where

import Prelude

import Concur.Core (Widget)
import Concur.VDom (HTML)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Concur.VDom.Run (runWidgetInDom)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Widgets.SimpleCalculatorWidget (widget)

hello :: forall a. Widget HTML a
hello = do
  void $ D.node "button" [P.handle "click"] [D.text "Say Hello"]
  D.text "Hello Sailor!"

main :: Effect Unit
main = launchAff_ do
  delay (Milliseconds 0.0)
  liftEffect $ runWidgetInDom "concur" widget
