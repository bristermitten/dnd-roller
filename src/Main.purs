module Main
  ( main
  , DiceRoll
  , DiceSet
  , Roll(..)
  , evalDice
  , parseDice
  , roll
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.List (List, fromFoldable)
import Data.List.Lazy (replicate)
import Data.Maybe (fromJust, fromMaybe, maybe, optional)
import Data.Traversable (sum, traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy, try, (<?>))
import Parsing.Language (emptyDef)
import Parsing.String (char)
import Parsing.Token (makeTokenParser)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (createElement)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element (toEventTarget, toNode) as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Node as Node
import Web.Event.Event (EventType(..))
import Web.Event.Event (preventDefault) as Event
import Web.Event.EventTarget (addEventListener) as Element
import Web.Event.EventTarget (eventListener) as Event
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as InputElement
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  htmlDoc <- document =<< window
  body <- maybe (throw "Could not find body element") pure =<< HTMLDocument.body htmlDoc
  let
    doc = HTMLDocument.toDocument htmlDoc

  formElem <- createElement "form" doc
  textInputElem <- createElement "input" doc
  submitElem <- createElement "input" doc
  resultElem <- createElement "p" doc

  setAttribute "type" "text" textInputElem
  setAttribute "name" "dice" textInputElem
  setAttribute "placeholder" "1d20+5" textInputElem

  setAttribute "type" "submit" submitElem
  setAttribute "value" "Roll" submitElem

  setAttribute "name" "result" resultElem
  setAttribute "readonly" "true" resultElem
  setAttribute "value" "Result will be shown here" resultElem

  -- click listener
  listener <- Event.eventListener
    ( \event -> do
        -- cancel normal event
        Event.preventDefault event
        -- not pretty, but it works
        dice <- InputElement.value (unsafePartial $ fromJust $ InputElement.fromElement textInputElem)
        result <- evalDice dice
        Node.setTextContent result (Element.toNode resultElem)
        pure unit
    )
  _ <- Element.addEventListener (EventType "click") listener true (Element.toEventTarget submitElem)

  let
    bodyNode = HTMLElement.toNode body
    formNode = Element.toNode formElem
    textInputNode = Element.toNode textInputElem
    submitNode = Element.toNode submitElem
    resultNode = Element.toNode resultElem

  appendChild textInputNode formNode
  appendChild submitNode formNode
  appendChild formNode bodyNode
  appendChild resultNode bodyNode

  pure unit

evalDice :: String -> Effect String
evalDice = parseDice >>> either (pure) (roll >>> map show)

parseDice :: String -> Either String DiceSet
parseDice = flip runParser diceSetParser >>> lmap show

integer :: Parser String Int
integer = (makeTokenParser emptyDef).integer <?> "integer"

diceSetParser :: Parser String DiceSet
diceSetParser = do
  dice <- join <$> rollParser `sepBy` char '+'
  pure dice

rollParser :: Parser String (List Roll)
rollParser = try dice <|> const
  where
  const = do
    modifier <- integer
    pure (fromFoldable [ Modifier modifier ])

  dice :: Parser String (List Roll)
  dice = do
    count <- optional integer
    sides <- char 'd' *> integer
    pure (fromFoldable $ replicate (fromMaybe 1 count) (DiceRoll { sides: sides }))

roll :: DiceSet -> Effect Int
roll diceSet = do
  rolls <- traverse rollDice diceSet
  pure (sum rolls)

rollDice :: Roll -> Effect Int
rollDice (Modifier i) = pure i
rollDice (DiceRoll diceRoll) = randomInt 1 (diceRoll.sides)

type DiceSet =
  List Roll

data Roll
  = DiceRoll DiceRoll
  | Modifier Int

instance Show Roll where
  show (DiceRoll diceRoll) = "d" <> show (diceRoll.sides)
  show (Modifier i) = show i

type DiceRoll = { sides :: Int }

