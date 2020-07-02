module Main where

import Prelude

import Concur.VDom.DOM as D
import Concur.VDom.Run (runWidgetInDom)
import Effect (Effect)
import SlowButtonList (hugeButtonListDemo)
import Widgets.SimpleCalculatorWidget (widget)

main :: Effect Unit
main = runWidgetInDom "concur" do
  D.node "div" []
    [ D.node "h2" [] [D.text "Simple Calculator"]
    , widget
    , D.node "h2" [] [D.text "Huge Button List"]
    , hugeButtonListDemo 10000
    ]
