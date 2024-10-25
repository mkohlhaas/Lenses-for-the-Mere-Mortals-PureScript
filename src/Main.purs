module Main where

import Prelude (class Show, Unit, flip, discard, show, ($), (#), (<<<), (>>>), (*))

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', lens, set, over, _1, _2)
import Data.Lens.At (at)
import Data.Lens.Getter (view)
import Data.List (List(..), (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T5, get1, get2, get3, get4, tuple4)
import Effect (Effect)
import Effect.Console (log)

------------------
-- Introduction --
------------------

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

--------------------------------
-- Ch. 1 - Tuples and Records --
--------------------------------

_first :: ∀ a b any. Lens (Tuple a any) (Tuple b any) a b
_first =
  lens getter setter
  where
  getter = fst -------------------------------- whole -> part (old value)
  setter (Tuple _ kept) new = Tuple new kept -- whole -> part (new value) -> whole

type Event =
  { subject :: String
  , object :: String
  , action :: String
  , count :: Int
  }

duringNetflix :: Event
duringNetflix =
  { subject: "Brian"
  , object: "Dawn"
  , action: "cafuné"
  , count: 0
  }

-- lens
_action :: ∀ a b any. Lens { action :: a | any } { action :: b | any } a b
_action =
  lens getter setter
  where
  getter = _.action
  setter whole new = whole { action = new }

-- lens
_count :: ∀ a b any. Lens { count :: a | any } { count :: b | any } a b
_count = lens _.count $ _ { count = _ }

-- nested data structures
both = Tuple "example" duringNetflix

_bothCount :: ∀ a b any1 any2. Lens (Tuple any1 { count :: a | any2 }) (Tuple any1 { count :: b | any2 }) a b
_bothCount = _2 <<< _count

------------------------------
-- 1.4 Composition exercise --
------------------------------

_object :: Lens' Event String
_object = lens _.object $ _ { object = _ }

stringified ∷ Tuple String String
stringified = over _2 show both

-- type Lens s t a b = …
--   stab
--   • s represents the type of the whole that functions like `set` take.
--   • t represents the type of the whole that `set` produces.
--   • a represents the original part given to `set` or `over`.
--   • b represents the type of the value that replaced the a.
-- Note: if b narrows to a different concrete type than a does, t will necessarily be a different type than s.
--
-- type Lens' s a = Lens s s a a

-- fourLong ∷ Tuple Int (Tuple Int (Tuple Int (Tuple Int Unit)))
fourLong ∷ T5 Int Int Int Int Unit
fourLong = tuple4 1 2 3 4

_elt1 = _1
_elt2 = _2 <<< _1
_elt3 = _2 <<< _2 <<< _1
_elt4 = _2 <<< _2 <<< _2 <<< _1

set1 = flip (set _elt1)
set2 = flip (set _elt2)
set3 = flip (set _elt3)
set4 = flip (set _elt4)

main :: Effect Unit
main = do
  log $ show $ viewAnimal 3838 model1 ------------------------ (Just (Animal { id: 0, name: "Genesis", tags: ("Mare" : Nil) }))
  log $ show $ view (_animal 3838) model1 -------------------- (Just (Animal { id: 0, name: "Genesis", tags: ("Mare" : Nil) }))
  log $ show $ view (_animal 3838) model2 -------------------- (Just (Animal { id: 1, name: "Monkey", tags: ("Ape" : Nil) }))
  -- log $ show $ over (_tags 3838) ((:) "new tag") model1
  log $ view _first $ Tuple "one" 1 -------------------------- one
  log $ show $ set _first "harangue" $ Tuple "one" 1 --------- (Tuple "harangue" 1)
  log $ show $ over _first String.toUpper $ Tuple "one" 1 ---- (Tuple "ONE" 1)
  log $ show $ over _first String.length $ Tuple "one" 1 ----- (Tuple 3 1)
  log $ show $ set _2 "no-longer-an-Int" $ Tuple "one" 1 ----- (Tuple "one" "no-longer-an-Int")
  log $ show $ set _1 "no-longer-one" $ Tuple "one" 1 -------- (Tuple "no-longer-one" 1)
  log $ show $ view _action duringNetflix -------------------- "cafuné"
  log $ show $ over _action String.toUpper duringNetflix ----- { action: "CAFUNÉ", count: 0,   object: "Dawn", subject: "Brian" }
  log $ show $ set _count 999 duringNetflix ------------------ { action: "cafuné", count: 999, object: "Dawn", subject: "Brian" }
  log $ show $ view _bothCount both -------------------------- 0
  log $ show $ (both # set _bothCount 55 # view _bothCount) -- 55
  log $ show $ 5 --------------------------------------------- 5
  log $ show $ show 5 ---------------------------------------- "5"
  log $ show $ show duringNetflix ---------------------------- "{ action: \"cafuné\", count: 0, object: \"Dawn\", subject: \"Brian\" }"
  log $ show $ over _2 show both ----------------------------- (Tuple "example" "{ action: \"cafuné\", count: 0, object: \"Dawn\", subject: \"Brian\" }")
  log $ show $ set _2 (view (_2 <<< _object) both) both ------ (Tuple "example" "Dawn")
  log $ show $ set _action 5 duringNetflix ------------------- { action: 5, count: 0, object: "Dawn", subject: "Brian" }
  log $ show $ set _action 5 { action: "figure" } ------------ { action: 5 }
  log $ show $ fourLong -------------------------------------- (Tuple 1 (Tuple 2 (Tuple 3 (Tuple 4 unit))))
  log $ show $ get1 fourLong --------------------------------- 1
  log $ show $ get2 fourLong --------------------------------- 2
  log $ show $ get3 fourLong --------------------------------- 3
  log $ show $ get4 fourLong --------------------------------- 4
  -- log $ show $ get5 fourLong -- compiler error
  log $ show $ set2 fourLong "TWOTWOTWO" --------------------- (Tuple 1 (Tuple "TWOTWOTWO" (Tuple 3 (Tuple 4 unit))))
  log $ show $ over _elt3 ((*) 60000) fourLong --------------- (Tuple 1 (Tuple 2 (Tuple 180000 (Tuple 4 unit))))
