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
    Start
  | MonoidPage
  | MonadPage
derive instance eqStoryEvent :: Eq StoryEvent

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
storyViewSt = P.classList $ map Just ["storyView"]

page1 :: StoryPage (Maybe Unit) StoryEvent
page1 = StoryPage {
  event : Start
, storyView : D.div [storyViewSt] [
    D.text "And so begins a tale. A tale of widgets, monads, and monoids."
  ]
, nextPage : (\_ _ -> D.div' [
    monoidPage <$ D.button [P.onClick] [D.text "What's a monoid?"]
  , monadPage <$ D.button [P.onClick] [D.text "What's a monad?"]
  ])
}

-- TODO add links to wikipedia for monoid, and two for monoid

monoidPage :: StoryPage (Maybe Unit) StoryEvent
monoidPage = StoryPage {
  event : MonoidPage
, storyView : D.div [storyViewSt] [
    D.text $ "A monoid is an algebraic structure with a "
      <> "single associative binary operation and an identity element. "
      <> "Examples include the real or natural numbers with either mutiplication "
      <> "(with 1 as the identity) or addition (with 0 as the identity), "
      <> "or strings with concatenation and the empty string \"\" as the identity."
  ]
, nextPage : (\evs _ ->
  if A.last evs ==  Just MonadPage
    then page1 <$ D.button [P.onClick] [D.text "Page 1 Again!"]
    else monadPage <$ D.button [P.onClick] [D.text "What's a monad?"] )
}

-- TODO : make "do" bold
monadPage :: StoryPage (Maybe Unit) StoryEvent
monadPage = StoryPage {
  event : MonadPage
, storyView : D.div [storyViewSt] [
    D.text $ "Without getting into the mathematical details, a monad is a construction "
      <> " that allows sequencing operations together, in a manner similar to typical "
      <> " procedural programming. In languages like PureScript and Haskell, it allows "
      <> " for the use of do-notation."
  ]
, nextPage : (\evs _ ->
  if A.last evs ==  Just MonoidPage
    then page1 <$ D.button [P.onClick] [D.text "Page 1 Again!"]
    else monoidPage <$ D.button [P.onClick] [D.text "What's a monoid?"] )
}
-- TODO: choose monad or monoid
-- TODO: then make the remaining option whatever is left

main :: Effect Unit
main = runWidgetInDom "root" (runPage [] Nothing page1)
