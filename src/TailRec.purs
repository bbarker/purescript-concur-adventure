module TailRec where

import Prelude

import Concur.Core (Widget)
import Concur.Core.Types (affAction)
import Concur.VDom (HTML)
import Concur.VDom.DOM as D
import Concur.VDom.Props as P
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)

data Tid
foreign import setTimeout :: Int -> Effect Unit -> Effect Tid
foreign import cancelTimeout :: Tid -> Effect Unit

delay :: forall v. Monoid v => Int -> Widget v Unit
delay duration = affAction \cb -> do
  _ <- setTimeout duration (runEffectFn1 cb (Right unit))
  pure mempty

tailRecDemo :: forall a. Widget HTML a
tailRecDemo = do
  D.node' "div"
    [ D.node' "p" [D.text ("This demo shows that tail recursion is stack safe in Concur (even without using tailRecM).")]
    , D.node' "p" [D.text "This widget performs a tail recursive call roughly once every 10 milliseconds, and will never exhaust the stack."]
    , do
        void $ D.node "button" [P.handle "click"] [D.text "Start Tail Recursion Demo"]
        tailRecWidget 0 2
    ]

tailRecWidget :: forall a. Int -> Int -> Widget HTML a
tailRecWidget count times = do
  stopRequested <- D.node' "div"
    [ D.text $ "Recursive call # " <> show count <> " "
    , true <$ D.node "button" [P.handle "click"] [D.text $ "Stop it!"]
    , false <$ delay 10
    ]
  if stopRequested
    then do
      D.node' "div"
        [ D.text ("Ran " <> show count <> " recursive calls. ")
        , D.node "button" [unit <$ P.handle "click"] [D.text "Restart"]
        , D.text (" (iteration # " <> show times <> ")?")
        ]
      tailRecWidget 0 (times + 1)
    else tailRecWidget (count+1) times
