module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.Nullable as Nullable
import Global.Unsafe (unsafeStringify)
import Simple.JSON (readJSON)

type MyThingy =
  { a :: String
  , b :: Array String
  }

myThingyJsonThing1 :: String
myThingyJsonThing1 =
  """
    { "a": "hello"
    , "b": null
    }
  """

myThingyJsonThing2 :: String
myThingyJsonThing2 =
  """
    { "a": "hello"
    , "b": []
    }
  """

-- by using modify here, the type that is being parsed here uses "b" :: Nullable (Array String) from simple-json!
-- you can look at the definition of modify in purescript-record to see how the types line up!
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty ::
  String -> Either (NonEmptyList ForeignError) MyThingy
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty str = do
  json <- readJSON str
  let b = fromMaybe [] <<< Nullable.toMaybe $ json.b
  pure $ json { b = b }


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log' $ parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty myThingyJsonThing1
  -- {"value0":{"b":[],"a":"hello"}}
  log' $ parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty myThingyJsonThing2
  -- {"value0":{"b":[],"a":"hello"}}
  where
    -- Show for records coming in 0.12 :)
    log' = log <<< unsafeStringify
