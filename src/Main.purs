module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)


-- | d is for Data, such as user input
-- | n is for names of past pages or events
type StoryPageRows d n = (
  event :: n
, storyView :: Widget HTML Void
, nextPage :: Array n -> Maybe d -> Widget HTML (StoryPage d n)
)

newtype StoryPage d n = StoryPage (Record (StoryPageRows d n))
derive instance newtypeStoryPage :: Newtype (StoryPage d n) _

---- Specific story implementation follows ----

data StoryEvent =
    One
  | Two
  | Three
  | Four

-- | This is like the "main" loop of the game. In this implementation, we
-- | always leave up the previous story view, below the current view,
-- | so readers or players can scroll down the page.
runPage :: forall d n. Array n -> Maybe d -> StoryPage d n -> Widget HTML Void
runPage evs mD sp = D.div [] [
    do
      nextP <- spR.nextPage evs mD
      runPage (addEv spR.event) Nothing nextP
  , spR.storyView
  ]
  where
    spR = unwrap sp
    addEv ev = A.snoc evs ev

storyViewSt :: ∀ a. P.ReactProps a
storyViewSt = P.classList $ map Just ["storystoryView"]

page1 :: StoryPage (Maybe Unit) StoryEvent
page1 = StoryPage {
  event : One
, storyView : D.div [storyViewSt] [
    D.text "And so begins a tale. A tale of widgets, monads, and monoids."
  ]
, nextPage : (\_ _ -> page1 <$ D.button [P.onClick] [D.text "Page 1 Again!"] ) -- TODO
}

-- TODO: choose monad or monoid
-- TODO: then make the remaining option whatever is left

main :: Effect Unit
main = runWidgetInDom "root" (runPage [] Nothing page1)
