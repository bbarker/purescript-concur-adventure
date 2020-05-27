module Widgets.SimpleCalculatorWidget where

import Concur.Core (Widget)
import Concur.VDom (HTML)
import Concur.VDom.DOM (node, text)
import Concur.VDom.Props as P
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$))
import SimpleCalculator as SimpleCalculator

calculatorWidget :: SimpleCalculator.Status -> Widget HTML SimpleCalculator.Key
calculatorWidget status = node "div" [] [
    node "div" [P.prop "class" "display"] [text status.display],
    node "table" [] [
        node "tbody" [] [
            node "tr" [] [
                key 1 ""          SimpleCalculator.K_7        "7",
                key 1 ""          SimpleCalculator.K_8        "8",
                key 1 ""          SimpleCalculator.K_9        "9",
                key 1 "operator"  SimpleCalculator.K_Divide   "/"
            ],
            node "tr" [] [
                key 1 ""          SimpleCalculator.K_4        "4",
                key 1 ""          SimpleCalculator.K_5        "5",
                key 1 ""          SimpleCalculator.K_6        "6",
                key 1 "operator"  SimpleCalculator.K_Multiple "*"
            ],
            node "tr" [] [
                key 1 ""          SimpleCalculator.K_1        "1",
                key 1 ""          SimpleCalculator.K_2        "2",
                key 1 ""          SimpleCalculator.K_3        "3",
                key 1 "operator"  SimpleCalculator.K_Subtract "-"
            ],
            node "tr" [] [
                key 2 ""          SimpleCalculator.K_0        "0",
                key 1 "decimal"   SimpleCalculator.K_Dot      ".",
                key 1 "operator"  SimpleCalculator.K_Add      "+"
            ],
            node "tr" [] [
                key 3 "result"    SimpleCalculator.K_Equal    "=",
                key 1 "cancel"    SimpleCalculator.K_C        "C"
            ]
        ]
    ]
]
    where
        key :: Int -> String -> SimpleCalculator.Key -> String -> Widget HTML SimpleCalculator.Key
        key colSpan className action label = action <$ node "td" [P.prop "colSpan" "", P.prop "class" className, P.handle "click"] [text label]

widget :: forall a. Widget HTML a
widget = go SimpleCalculator.initialState
    where
        go :: SimpleCalculator.Status -> Widget HTML a
        go status = do
            key <- calculatorWidget status
            go $ SimpleCalculator.handleKey status key
