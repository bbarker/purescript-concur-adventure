module SlowButtonList where

import Prelude

import Concur.Core (Widget)
import Concur.VDom (HTML)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Control.Alt ((<|>))
import Data.Array ((..))

hugeButtonListDemo :: Int -> Widget HTML Unit
hugeButtonListDemo num = do
  slow <- D.node' "div"
    [ D.text $ "Show a list of " <> show num <> " buttons"
    , D.node "button" [P.handle "click"] [D.text "SLOW list"]
    ]
  let arr = (1 .. num)
  n <- slowButtonList arr
  D.text ("You clicked button#" <> show n) <|> D.node "button" [unit <$ P.handle "click"] [D.text "Restart?"]
  hugeButtonListDemo num

-- Slower but more idiomatic list of D.node "button"s
-- Simply use the standard D.node "button" widget and compose together in a div
slowButtonList :: Array Int -> Widget HTML Int
slowButtonList = D.node' "div" <<< map buttonize
  where buttonize n = D.node "button" [n <$ P.handle "click"] [D.text (show n)]

-- Use a lower level interface to create a large number of D.node "button" views manually
-- This is slightly better than the slow version, because it doesn't create individual aff actions for each D.node "button".
-- fastButtonList :: Array Int -> Widget HTML Int
-- fastButtonList arr = mkLeafWidget (\h -> map (\i -> D.D.node "button" [P.P.handle "click" (const (h i))] [D.D.text (show i)]) arr)
