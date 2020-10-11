module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
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
  | WidgetPage
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

monoidPage :: StoryPage (Maybe Unit) StoryEvent
monoidPage = StoryPage {
  event : MonoidPage
, storyView : D.div [storyViewSt] [
    D.text "A " <|> monoidLink "monoid" <|> D.text " is an algebraic structure with a "
      <|> D.text "single associative binary operation and an identity element. "
      <|> D.text "Examples include the real or natural numbers with either mutiplication "
      <|> D.text "(with 1 as the identity) or addition (with 0 as the identity), "
      <|> D.text "or strings with concatenation and the empty string ("
      <|> D.code' [D.text "\"\""] <|> D.text ") as the identity."
  ]
, nextPage : (\evs _ ->
  if A.last evs ==  Just MonadPage
    then widgetPage <$ D.button [P.onClick] [D.text "And Widgets?"]
    else monadPage <$ D.button [P.onClick] [D.text "What's a monad?"] )
}

monadPage :: StoryPage (Maybe Unit) StoryEvent
monadPage = StoryPage {
  event : MonadPage
, storyView : D.div [storyViewSt] [
    D.text "Without getting into the " <|> monadCatLink "mathematical details"
      <|> D.text ", a " <|> monadFpLink "monad" <|> D.text " is a construction "
      <|> D.text "that allows sequencing operations together"
      <|> D.text ", in a manner similar to typical "
      <|> D.text "procedural programming. In languages "
      <|> D.text "like PureScript and Haskell, it allows "
      <|> D.text "for the use of " <|> D.em' [D.text "do"] <|> D.text "-notation."
  ]
, nextPage : (\evs _ ->
  if A.last evs ==  Just MonoidPage
    then widgetPage <$ D.button [P.onClick] [D.text "And Widgets?"]
    else monoidPage <$ D.button [P.onClick] [D.text "What's a monoid?"] )
}

widgetPage :: StoryPage (Maybe Unit) StoryEvent
widgetPage = StoryPage {
  event : WidgetPage
, storyView : D.div [storyViewSt] [
    D.text "In " <|> concurDocLink "Concur"
      <|> D.text ", Widgets are both monads and monoids. They sequence together "
      <|> D.text "actions, such as user-input, "
      <|> D.text "via their monadic properties, and widgets, are "
      <|> D.text "merged together through their monoidal operation. "
      <|> D.text "In other words, they allow "
      <|> D.text "algebraic construction of widgets in "
      <|> D.text "time (monadic) and space (monoidal)."
  ]
, nextPage : (\_ _ ->  page1 <$ D.button [P.onClick] [D.text "Page 1 Again!"])
}

monoidLink :: String -> forall a. Widget HTML a
monoidLink = makeLink "https://en.wikipedia.org/wiki/Monoid"

monadCatLink :: String -> forall a. Widget HTML a
monadCatLink = makeLink "https://en.wikipedia.org/wiki/Monad_(category_theory)"

monadFpLink :: String -> forall a. Widget HTML a
monadFpLink = makeLink "https://en.wikipedia.org/wiki/Monad_(functional_programming)"

makeLink :: String -> String -> forall a. Widget HTML a
makeLink urlStr str = D.a_ [P.href urlStr, P.target "_blank"] (D.text str)

concurDocLink :: String -> forall a. Widget HTML a
concurDocLink = makeLink "https://github.com/ajnsit/concur-documentation/blob/master/README.md"

main :: Effect Unit
main = runWidgetInDom "root" (runPage [] Nothing page1)
