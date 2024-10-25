module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (lens, over)
import Data.Lens.At (at)
import Data.Lens.Getter (view)
import Data.List (List(..), (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude as List

data Animal = Animal { id :: Int, name :: String, tags :: List String }
type Model = { aField :: Int, animals :: Map Int Animal, otherField :: Int }

derive instance Generic Animal _
instance Show Animal where
  show = genericShow

insertMap ∷ Int → String → String → Map Int Animal
insertMap val name tag = insert val (Animal { id: 0, name, tags: (tag : Nil) }) empty

model1 ∷ Model
model1 = { aField: 0, animals: insertMap 3838 "Genesis" "Mare", otherField: 1 }

viewAnimal :: Int -> Model -> Maybe Animal
viewAnimal id = _.animals >>> lookup id

-- view :: (Model -> Maybe Animal) -> Model -> Maybe Animal
-- view optic whole = optic whole

-- optic
-- _animal ∷ Int → Model → Maybe Animal
-- _animal = viewAnimal

newAnimal = (Animal { id: 1, name: "Monkey", tags: ("Ape" : Nil) })

-- Replace animal
model2 :: Model
model2 = model1 { animals = insert 3838 newAnimal model1.animals }

-- Using Lens

-- lens
_animals = lens _.animals $ _ { animals = _ }

-- optic
_animal id = _animals <<< at id

main :: Effect Unit
main = do
  log $ show $ viewAnimal 3838 model1
  log $ show $ view (_animal 3838) model1
  log $ show $ view (_animal 3838) model2
-- log $ show $ over (_animal 3838) (map $ List.append "new tag") model1
