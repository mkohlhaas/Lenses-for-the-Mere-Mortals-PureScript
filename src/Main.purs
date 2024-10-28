module Main where

import Color (Color)
import Color as Color
import Critter4Us.Main as Critter
import Critter4UsRefactored.Main as CritterRf
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Lens (Lens, Lens', _1, _2, lens, over, set, setJust)
import Data.Lens.At (at)
import Data.Lens.Common (_Right)
import Data.Lens.Fold (firstOf, lastOf, preview, toListOf)
import Data.Lens.Getter (view)
import Data.Lens.Index (ix)
import Data.Lens.Prism (Prism', is, isn't, nearly, only, prism, prism', review)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Traversal (Traversal', element, traversed)
import Data.List (List(..), (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (class Ord)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (T5, get1, get2, get3, get4, tuple4)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Prelude (class Show, Unit, discard, flip, identity, map, negate, show, unit, (#), ($), (*), (<#>), (<<<), (<>), (==), (>>>))

------------------
-- Introduction --
------------------

data Animal = Animal { id ∷ Int, name ∷ String, tags ∷ List String }
type Model = { aField ∷ Int, animals ∷ Map Int Animal, otherField ∷ Int }

derive instance Generic Animal _
instance Show Animal where
  show = genericShow

insertMap ∷ Int → String → String → Map Int Animal
insertMap val name tag = insert val (Animal { id: 0, name, tags: (tag : Nil) }) empty

model1 ∷ Model
model1 = { aField: 0, animals: insertMap 3838 "Genesis" "Mare", otherField: 1 }

viewAnimal ∷ Int → Model → Maybe Animal
viewAnimal id = _.animals >>> lookup id

-- view ∷ (Model → Maybe Animal) → Model → Maybe Animal
-- view optic whole = optic whole

-- optic
-- _animal ∷ Int → Model → Maybe Animal
-- _animal = viewAnimal

newAnimal = (Animal { id: 1, name: "Monkey", tags: ("Ape" : Nil) })

-- Replace animal
model2 ∷ Model
model2 = model1 { animals = insert 3838 newAnimal model1.animals }

-- Using Lens

-- lens
_animals = lens _.animals $ _ { animals = _ }

-- optic
_animal id = _animals <<< at id -- `id` is key value in map

-- _tags id = _animals <<< at id <<< _.tags

--------------------------------
-- Ch. 1 - Tuples and Records --
--------------------------------
-- Synopsis
-- Types: Lens, Lens'
-- Constructors: lens
-- Functions: view, set, over
-- Predefined optics: _1, _2

_first ∷ ∀ a b any. Lens (Tuple a any) (Tuple b any) a b
_first =
  lens getter setter
  where
  getter = fst -------------------------------- whole → part (old value)
  setter (Tuple _ kept) new = Tuple new kept -- whole → part (new value) → whole

type Event =
  { subject ∷ String
  , object ∷ String
  , action ∷ String
  , count ∷ Int
  }

duringNetflix ∷ Event
duringNetflix =
  { subject: "Brian"
  , object: "Dawn"
  , action: "cafuné"
  , count: 0
  }

-- lens
_action ∷ ∀ a b any. Lens { action ∷ a | any } { action ∷ b | any } a b
_action =
  lens getter setter
  where
  getter = _.action
  setter whole new = whole { action = new }

-- lens
_count ∷ ∀ a b any. Lens { count ∷ a | any } { count ∷ b | any } a b
_count = lens _.count $ _ { count = _ }

-- nested data structures
both = Tuple "example" duringNetflix

_bothCount ∷ ∀ a b any1 any2. Lens (Tuple any1 { count ∷ a | any2 }) (Tuple any1 { count ∷ b | any2 }) a b
_bothCount = _2 <<< _count

------------------------------
-- 1.4 Composition exercise --
------------------------------

_object ∷ Lens' Event String
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

------------------------------
-- Ch. 2 - Maps (At lenses) --
------------------------------

-- Synopsis
-- Types: class At (a Lens)
-- Constructors: at
-- Functions: setJust
-- Predefined optics:

_key ∷ ∀ focus. Lens' (Map String focus) (Maybe focus)
_key = lens getter setter
  where
  getter = Map.lookup "key"
  setter whole wrapped =
    case wrapped of
      Just new → Map.insert "key" new whole
      Nothing → Map.delete "key" whole

_atKey ∷ ∀ key focus. Ord key ⇒ key → Lens' (Map key focus) (Maybe focus)
_atKey key = lens getter setter
  where
  getter = Map.lookup key
  setter whole wrapped =
    case wrapped of
      Just new → Map.insert key new whole
      Nothing → Map.delete key whole

-- All optics are, in the end, a function from one profunctor to another:
--   ∀ blah blah blah. blah blah ⇒ blah blah ⇒
--   profunctor a b → profunctor s t
-- Naming the profunctor profunctor makes the type easier to read.
-- Most people name profunctors `p`.

composed = _1 <<< _2 <<< at 3

threeDeep = Tuple (Tuple "_1_" $ Map.singleton 3 "match") "_2_"

------------------------------------------
-- 3. Optics and refactoring (optional) --
------------------------------------------

---------------------------------------------------
-- 4. Operating on whole collections (Traversal) --
---------------------------------------------------

-- Synopsis
-- Types: Traversal , Traveral'
-- Constructors:
-- Functions: firstOf, lastOf, preview, toListOf
-- Predefined optics: traversed

_element1 :: ∀ a. Traversable a => Traversal' (a String) String
_element1 = element 1 traversed

-- at traversed trick (you must interpose a `traversed` to "unwrap" the Maybe result):
-- at 3 <<< _1                  ✗
-- at 3 <<< traversed <<< _1    ✓
-- at 3 <<< _Just     <<< _1    ✓   (_Just works too)

tupleMap = Map.fromFoldable
  [ (Tuple 3 (Tuple 8 "s"))
  , (Tuple 4 (Tuple 1 "_2_"))
  ]

mapArray =
  [ Map.singleton 3 "3"
  , Map.empty
  , Map.singleton 4 "4"
  ]

--------------------------------------------------------------
-- 5. Single elements in arrays and fixed-size maps (Index) --
--------------------------------------------------------------

-- Synopsis
-- Types: class Index (a Traversal)
-- Constructors: ix
-- Functions: preview, set, over
-- Predefined optics:

-- For many types, the resulting Traversal of
-- `ix 1` is more efficient than one created with
-- `element 1 traversed`.

-- An At lens can be used to create or delete focus elements.
-- You use Index optics when the underlying type either prevents that or you don’t want it even if it’s allowed.

-- Because `at 1` is a lens, you use `view`. Because `ix 1` is a traversal, you use `preview`.

---------------------------
-- 6. Sum types (prisms) --
---------------------------

-- Synopsis
-- Types: Prism, Prism'
-- Constructors: prism, prism', only, nearly
-- Functions: preview, review (means: reverse view), is, isn't
-- Predefined optics: _Right, _Left, _Nothing, _Just

newtype Percent = Percent Number
data Point = Point Number Number

data Fill
  = Solid Color
  | LinearGradient Color Color Percent
  | RadialGradient Color Color Point
  | NoFill

_solidFill :: Prism' Fill Color
_solidFill = prism' constructor focuser -- prism' uses Maybe
  where
  constructor = Solid
  focuser fill = case fill of
    Solid color -> Just color
    _ -> Nothing

-- the same
_solidFill' :: Prism' Fill Color
_solidFill' = prism' Solid case _ of
  Solid color -> Just color
  _ -> Nothing

-- the same
_anotherSolidFill :: Prism' Fill Color
_anotherSolidFill = prism Solid case _ of -- prism uses Either
  Solid color -> Right color
  otherCases -> Left otherCases

-- The resulting Prism has Unit as its focus type.
-- `only` requires that the sum type implement class Eq.
_solidWhite = only (Solid Color.white) -- `only` tests for a single value

-- `nearly` is same as `only` but doesn’t require Eq
_solidWhite' :: Prism' Fill Unit
_solidWhite' =
  nearly (Solid Color.white) case _ of
    Solid color -> color == Color.white -- Color implements Eq so this is trivial
    _ -> false

fillRadial :: Fill
fillRadial = RadialGradient Color.white Color.black $ Point 1.0 3.4

_centerPoint :: Prism' Fill Point
_centerPoint = prism' constructor focuser
  where
  constructor point =
    RadialGradient Color.black Color.white point -- colors are hard-coded and break prism laws: The preview-review round trip doesn't work.
  focuser = case _ of
    RadialGradient _ _ point -> Just point
    _ -> Nothing

type RadialInterchange =
  { color1 :: Color
  , color2 :: Color
  , center :: Point
  }

_centerPoint' :: Prism' Fill RadialInterchange
_centerPoint' = prism constructor focuser
  where
  constructor { color1, color2, center } =
    RadialGradient color1 color2 center
  focuser = case _ of
    RadialGradient color1 color2 center -> Right { color1, color2, center }
    otherCases -> Left otherCases

_intSource :: Prism' String String
_intSource =
  prism' identity focuser
  where
  focuser s = case (fromString s) of
    Just _ -> Just s
    Nothing -> Nothing

_int :: Prism' String Int
_int =
  prism' show focuser
  where
  focuser s = case (fromString s) of
    Just i -> Just i
    Nothing -> Nothing

derive newtype instance Eq Percent
derive instance Generic Percent _
instance showPercent :: Show Percent where
  show (Percent f) = "(" <> show f <> "%)"

derive instance Eq Point
derive instance Generic Point _
instance showPoint :: Show Point where
  show (Point x y) = "(" <> show x <> ", " <> show y <> ")"

derive instance Eq Fill
derive instance Generic Fill _
instance Show Fill where
  show x = genericShow x

main ∷ Effect Unit
main = do
  log $ show $ viewAnimal 3838 model1 ------------------------------------------------------------------------ (Just (Animal { id: 0, name: "Genesis", tags: ("Mare" : Nil) }))
  log $ show $ view (_animal 3838) model1 -------------------------------------------------------------------- (Just (Animal { id: 0, name: "Genesis", tags: ("Mare" : Nil) }))
  log $ show $ view (_animal 3838) model2 -------------------------------------------------------------------- (Just (Animal { id: 1, name: "Monkey", tags: ("Ape" : Nil) }))
  -- log $ show $ over (_animal 3838) (map (:) "new tag") model1
  log $ view _first $ Tuple "one" 1 -------------------------------------------------------------------------- one
  log $ show $ set _first "harangue" $ Tuple "one" 1 --------------------------------------------------------- (Tuple "harangue" 1)
  log $ show $ over _first String.toUpper $ Tuple "one" 1 ---------------------------------------------------- (Tuple "ONE" 1)
  log $ show $ over _first String.length $ Tuple "one" 1 ----------------------------------------------------- (Tuple 3 1)
  log $ show $ set _2 "no-longer-an-Int" $ Tuple "one" 1 ----------------------------------------------------- (Tuple "one" "no-longer-an-Int")
  log $ show $ set _1 "no-longer-one" $ Tuple "one" 1 -------------------------------------------------------- (Tuple "no-longer-one" 1)
  log $ show $ view _action duringNetflix -------------------------------------------------------------------- "cafuné"
  log $ show $ over _action String.toUpper duringNetflix ----------------------------------------------------- { action: "CAFUNÉ", count: 0,   object: "Dawn", subject: "Brian" }
  log $ show $ set _count 999 duringNetflix ------------------------------------------------------------------ { action: "cafuné", count: 999, object: "Dawn", subject: "Brian" }
  log $ show $ view _bothCount both -------------------------------------------------------------------------- 0
  log $ show $ (both # set _bothCount 55 # view _bothCount) -------------------------------------------------- 55
  log $ show $ 5 --------------------------------------------------------------------------------------------- 5
  log $ show $ show 5 ---------------------------------------------------------------------------------------- "5"
  log $ show $ show duringNetflix ---------------------------------------------------------------------------- "{ action: \"cafuné\", count: 0, object: \"Dawn\", subject: \"Brian\" }"
  log $ show $ over _2 show both ----------------------------------------------------------------------------- (Tuple "example" "{ action: \"cafuné\", count: 0, object: \"Dawn\", subject: \"Brian\" }")
  log $ show $ set _2 (view (_2 <<< _object) both) both ------------------------------------------------------ (Tuple "example" "Dawn")
  log $ show $ set _action 5 duringNetflix ------------------------------------------------------------------- { action: 5, count: 0, object: "Dawn", subject: "Brian" }
  log $ show $ set _action 5 { action: "figure" } ------------------------------------------------------------ { action: 5 }
  log $ show $ fourLong -------------------------------------------------------------------------------------- (Tuple 1 (Tuple 2 (Tuple 3 (Tuple 4 unit))))
  log $ show $ get1 fourLong --------------------------------------------------------------------------------- 1
  log $ show $ get2 fourLong --------------------------------------------------------------------------------- 2
  log $ show $ get3 fourLong --------------------------------------------------------------------------------- 3
  log $ show $ get4 fourLong --------------------------------------------------------------------------------- 4
  -- log $ show $ get5 fourLong -- (expected) compiler error 
  log $ show $ set2 fourLong "TWOTWOTWO" --------------------------------------------------------------------- (Tuple 1 (Tuple "TWOTWOTWO" (Tuple 3 (Tuple 4 unit))))
  log $ show $ over _elt3 ((*) 60000) fourLong --------------------------------------------------------------- (Tuple 1 (Tuple 2 (Tuple 180000 (Tuple 4 unit))))
  log $ show $ Map.insert "key" 5 Map.empty ------------------------------------------------------------------ (fromFoldable [(Tuple "key" 5)])
  log $ show $ Map.delete "key" $ Map.singleton "key" 5 ------------------------------------------------------ (fromFoldable [])
  log $ show $ set (at "key") (Just 3) $ Map.empty ----------------------------------------------------------- (fromFoldable [(Tuple "key" 3)])
  log $ show $ set (at "key") (Just 3) $ Object.empty -------------------------------------------------------- (fromFoldable [(Tuple "key" 3)]) 
  log $ show $ setJust (at "key") 3 $ Map.empty -------------------------------------------------------------- (fromFoldable [(Tuple "key" 3)]) 
  log $ show $ over (at "key") (map negate) $ Map.singleton "key" 3 ------------------------------------------ (fromFoldable [(Tuple "key" -3)]) 
  log $ show $ over (at "KEY") (map negate) $ Map.singleton "key" 3 ------------------------------------------ (fromFoldable [(Tuple "key" 3)]) 
  log $ show $ view (at 1) $ Set.singleton 1 ----------------------------------------------------------------- (Just unit)
  log $ show $ view (at 2) $ Set.singleton 1 ----------------------------------------------------------------- Nothing
  log $ show $ isJust $ view (at 1) $ Set.singleton 1 -------------------------------------------------------- true
  log $ show $ set (at 1) Nothing $ Set.singleton 1 ---------------------------------------------------------- (fromFoldable [])
  log $ show $ set (at 'b') (Just unit) $ Set.singleton 'a' -------------------------------------------------- (fromFoldable ['a','b'])
  log $ show $ setJust (at 'b') unit $ Set.singleton 'a' ----------------------------------------------------- (fromFoldable ['a','b'])
  log $ show $ view composed threeDeep ----------------------------------------------------------------------- (Just "match")
  log $ show $ over composed (map String.toUpper) threeDeep -------------------------------------------------- (Tuple (Tuple "_1_" (fromFoldable [(Tuple 3 "MATCH")])) "_2_")
  log $ show $ Critter.initialModel -------------------------------------------------------------------------- { animals: (fromFoldable [(Tuple 3838 { id: 3838, name: "Genesis", tags: ["mare"] })]) }
  log $ show $ Critter.update Critter.initialModel (Critter.AddAnimal 1 "Bossy") ----------------------------- { animals: (fromFoldable [(Tuple 1 { id: 1, name: "Bossy", tags: [] }),(Tuple 3838 { id: 3838, name: "Genesis", tags: ["mare"] })]) }
  log $ show $ CritterRf.update CritterRf.initialModel (CritterRf.AddAnimal 1 "Bossy") ----------------------- { animals: (fromFoldable [(Tuple 1 { id: 1, name: "Bossy"           }),(Tuple 3838 { id: 3838, name: "Genesis"                 })]), tagDb: { idsByTag: (fromFoldable [(Tuple "mare" [3838])]), tagsById: (fromFoldable [(Tuple 3838 ["mare"])]) } }
  log $ show $ Critter.update Critter.initialModel (Critter.AddTag 3838 "skittish") -------------------------- { animals: (fromFoldable [(Tuple 3838 { id: 3838, name: "Genesis", tags: ["mare","skittish"] })]) }
  log $ show $ CritterRf.update CritterRf.initialModel (CritterRf.AddTag 3838 "skittish") -------------------- { animals: (fromFoldable [(Tuple 3838 { id: 3838, name: "Genesis"                            })]), tagDb: { idsByTag: (fromFoldable [(Tuple "mare" [3838]),(Tuple "skittish" [3838])]), tagsById: (fromFoldable [(Tuple 3838 ["mare","skittish"])]) } }
  let m = CritterRf.update CritterRf.initialModel (CritterRf.AddTag 3838 "skittish")
  log $ show $ CritterRf.tagsFor 3838 m.tagDb ---------------------------------------------------------------- ["mare","skittish"]
  log $ show $ CritterRf.idsFor "skittish" m.tagDb ----------------------------------------------------------- [3838]
  log $ show $ CritterRf.idsFor "missing" m.tagDb ------------------------------------------------------------ []
  log $ show $ over traversed negate [ 1, 2 ] ---------------------------------------------------------------- [-1,-2]
  log $ show $ over traversed negate ([] :: Array Unit) ------------------------------------------------------ []
  log $ show $ set traversed 99 [ 1, 2 ] --------------------------------------------------------------------- [99,99]
  log $ show $ over traversed negate (Just 3) ---------------------------------------------------------------- (Just -3)
  log $ show $ over traversed negate $ (Right 88 :: Either Unit Int) ----------------------------------------- (Right -88)
  log $ show $ over traversed negate $ (Left 88 :: Either Int Unit) ------------------------------------------ (Left 88)
  log $ show $ set traversed 3 Nothing ----------------------------------------------------------------------- Nothing
  log $ show $ toListOf traversed [ 1, 2, 3 ] ---------------------------------------------------------------- (1 : 2 : 3 : Nil)
  log $ show $ toListOf traversed (Just 3) ------------------------------------------------------------------- (3 : Nil)
  log $ show $ toListOf traversed $ (Right 3 :: Either Unit Int) --------------------------------------------- (3 : Nil)
  log $ show $ toListOf traversed $ (Left 3 :: Either Int Unit) ---------------------------------------------- Nil
  log $ show $ over traversed negate (1 : 2 : 3 : Nil) ------------------------------------------------------- (-1 : -2 : -3 : Nil)
  log $ show $ view traversed [ "D", "a", "w", "n" ] --------------------------------------------------------- "Dawn"
  log $ show $ view (traversed <<< _1) [ (Tuple "1" 1), (Tuple "23" 2) ] ------------------------------------- "123"
  -- log $ show $ view (traversed <<< _1) [ (Tuple 1 1), (Tuple 23 2) ] -------------------------------------- compiler error: Int is not a Monoid
  log $ show $ (Additive 1) <> (Additive 2) ------------------------------------------------------------------ (Additive 3)
  log $ show $ view traversed $ map Additive [ 1, 2, 3 ] ----------------------------------------------------- (Additive 6)
  log $ show $ view traversed $ over traversed Additive [ 1, 2, 3 ] ------------------------------------------ (Additive 6)
  log $ show $ view (traversed <<< traversed) $ over (traversed <<< traversed) Additive [ [ 1 ], [ 2, 3 ] ] -- (Additive 6)
  log $ show $ view traversed [ [ 1, 2 ], [ 3, 4 ] ] --------------------------------------------------------- [1,2,3,4] 
  log $ show $ firstOf traversed [ 1, 2, 3 ] ----------------------------------------------------------------- (Just 1)
  log $ show $ lastOf traversed [ 1, 2, 3 ] ------------------------------------------------------------------ (Just 3)
  log $ show $ over _element1 String.toUpper [ "no", "yes!", "no" ] ------------------------------------------ ["no","YES!","no"]
  log $ show $ over _element1 String.toUpper ("no" : "yes!" : "no" : Nil) ------------------------------------ ("no" : "YES!" : "no" : Nil)
  log $ show $ firstOf _element1 [ "no", "yes!", "no" ] ------------------------------------------------------ (Just "yes!")
  log $ show $ preview _element1 [ "no", "yes!", "no" ] ------------------------------------------------------ (Just "yes!")
  log $ show $ over (traversed <<< traversed) negate [ [ 1, 2 ], [ 3, 4 ] ] ---------------------------------- [[-1,-2],[-3,-4]]
  log $ show $ view traversed [ [ "1" ], [ "2", "3" ] ] ------------------------------------------------------ ["1","2","3"]
  log $ show $ view (traversed <<< traversed) [ [ "1" ], [ "2", "3" ] ] -------------------------------------- "123"
  log $ show $ over (traversed <<< _1) ((*) 1111) [ Tuple 1 2, Tuple 3 4 ] ----------------------------------- [(Tuple 1111 2),(Tuple 3333 4)]
  log $ show $ toListOf (traversed <<< _1) [ Tuple 1 2, Tuple 3 4 ] ------------------------------------------ (1 : 3 : Nil)
  log $ show $ preview (traversed <<< _1) [ Tuple 1 2, Tuple 3 4 ] ------------------------------------------- (Just 1)
  log $ show $ over (_1 <<< traversed) negate (Tuple [ 1, 2, 3 ] "don't care") ------------------------------- (Tuple [-1,-2,-3] "don't care")
  log $ show $ preview (at 3 <<< traversed <<< _1) tupleMap -------------------------------------------------- (Just 8)
  log $ show $ preview (at 3 <<< _Just <<< _1) tupleMap ------------------------------------------------------ (Just 8)
  log $ show $ set (at 3 <<< traversed <<< _1) 1234 tupleMap ------------------------------------------------- (fromFoldable [(Tuple 3 (Tuple 1234 "s")),(Tuple 4 (Tuple 1 "_2_"))])
  log $ show $ toListOf (traversed <<< at 3) mapArray -------------------------------------------------------- ((Just "3") : Nothing : Nothing : Nil)
  log $ show $ set (traversed <<< at 3) (Just "NEW") mapArray ------------------------------------------------ [(fromFoldable [(Tuple 3 "NEW")]),
  ------------------------------------------------------------------------------------------------------------  (fromFoldable [(Tuple 3 "NEW")]),
  ------------------------------------------------------------------------------------------------------------  (fromFoldable [(Tuple 3 "NEW"),(Tuple 4 "4")])]
  log $ show $ over (traversed <<< at 3) (map $ \s -> s <> "!+!" <> s) mapArray ------------------------------ [(fromFoldable [(Tuple 3 "3!+!3")]),
  ------------------------------------------------------------------------------------------------------------  (fromFoldable []),
  ------------------------------------------------------------------------------------------------------------  (fromFoldable [(Tuple 4 "4")])]
  log $ show $ toListOf _1 (Tuple 8 "hi") -------------------------------------------------------------------- (8 : Nil)
  log $ show $ preview _1 (Tuple 8 "hi") --------------------------------------------------------------------- (Just 8)
  log $ show $ view (ix 1) $ Map.singleton 1 "a" ------------------------------------------------------------- "a"
  log $ show $ view (ix 2) $ Map.singleton 1 "a" ------------------------------------------------------------- ""
  log $ show $ preview (ix 1) $ Map.singleton 1 "a" ---------------------------------------------------------- (Just "a")
  log $ show $ preview (ix 2) $ Map.singleton 1 "a" ---------------------------------------------------------- Nothing
  log $ show $ view (at 1) $ Map.singleton 1 "a" ------------------------------------------------------------- (Just "a") 
  log $ show $ view (at 2) $ Map.singleton 1 "a" ------------------------------------------------------------- Nothing
  log $ show $ preview (at 1) $ Map.singleton 1 "a" ---------------------------------------------------------- (Just (Just "a"))
  log $ show $ preview (at 2) $ Map.singleton 1 "a" ---------------------------------------------------------- (Just Nothing)
  log $ show $ set (at 1) (Just "new value") $ Map.singleton 1 "a" ------------------------------------------- (fromFoldable [(Tuple 1 "new value")])
  log $ show $ set (ix 1) "new value" $ Map.singleton 1 "a" -------------------------------------------------- (fromFoldable [(Tuple 1 "new value")])
  log $ show $ set (at 1) (Just "new value") $ Map.empty ----------------------------------------------------- (fromFoldable [(Tuple 1 "new value")])
  log $ show $ set (ix 1) "new value" $ Map.empty ------------------------------------------------------------ (fromFoldable [])
  log $ show $ over (at 1) (map String.toUpper) $ Map.singleton 1 "a" ---------------------------------------- (fromFoldable [(Tuple 1 "A")])
  log $ show $ over (ix 1) String.toUpper $ Map.singleton 1 "a" ---------------------------------------------- (fromFoldable [(Tuple 1 "A")])
  log $ show $ over (ix 1) String.toUpper $ Map.singleton 2 "unchanged" -------------------------------------- (fromFoldable [(Tuple 2 "unchanged")])
  log $ show $ preview (ix 1) [ 1, 2 ] ----------------------------------------------------------------------- (Just 2)
  log $ show $ view (ix 1) [ "1", "2" ] ---------------------------------------------------------------------- "2" (must be a monoid)
  log $ show $ set (ix 1) "ignored" [] ----------------------------------------------------------------------- []
  log $ show $ over (ix 1) negate [ 0, 1 ] ------------------------------------------------------------------- [0,-1]
  log $ show $ preview (traversed <<< ix 1) $ [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] ----------------------------------- (Just 2)
  log $ show $ view (traversed <<< ix 1) $ [ [ "1", "2", "3" ], [ "4", "5", "6" ] ] -------------------------- "25"
  log $ show $ preview (ix 1 <<< traversed) $ [ [ 1, 2, 3, 4, 5 ], [ 6, 7, 8, 9, 0 ] ] ----------------------- (Just 6)
  log $ show $ preview _solidFill $ Solid Color.white -------------------------------------------------------- (Just rgba 255 255 255 1.0)
  log $ show $ preview _anotherSolidFill $ Solid Color.white ------------------------------------------------- (Just rgba 255 255 255 1.0) -- prism uses Either but result is still a Maybe
  log $ show $ preview _solidFill NoFill --------------------------------------------------------------------- Nothing
  log $ show $ preview _anotherSolidFill NoFill -------------------------------------------------------------- Nothing
  log $ show $ review _solidFill Color.white ----------------------------------------------------------------- (Solid rgba 255 255 255 1.0)
  log $ show $ (is _solidFill $ Solid Color.white :: Boolean) ------------------------------------------------ true
  log $ show $ (is _solidFill $ NoFill :: Boolean) ----------------------------------------------------------- false
  log $ show $ (isn't _solidFill $ Solid Color.white :: Boolean) --------------------------------------------- false
  log $ show $ set _solidFill Color.white $ Solid Color.black ------------------------------------------------ (Solid rgba 255 255 255 1.0)
  log $ show $ review _solidFill Color.white ----------------------------------------------------------------- (Solid rgba 255 255 255 1.0)
  log $ show $ set (_1 <<< _solidFill) Color.white $ Tuple (Solid Color.black) 5 ----------------------------- (Tuple (Solid rgba 255 255 255 1.0) 5)
  log $ show $ preview _solidWhite (Solid Color.white) ------------------------------------------------------- (Just unit)
  log $ show $ review _solidWhite unit ----------------------------------------------------------------------- (Solid rgba 255 255 255 1.0)
  log $ show $ review _solidWhite' unit ---------------------------------------------------------------------- (Solid rgba 255 255 255 1.0)
  log $ show $ Color.white # review _solidFill # preview _solidFill ------------------------------------------ (Just rgba 255 255 255 1.0) - review-preview prism law
  log $ show $ Solid Color.white # preview _solidFill -------------------------------------------------------- (Just rgba 255 255 255 1.0)
  log $ show $ Solid Color.white # preview _solidFill <#> review _solidFill ---------------------------------- (Just (Solid rgba 255 255 255 1.0)) - preview-review prism law
  log $ show $ preview _centerPoint fillRadial --------------------------------------------------------------- (Just (1.0, 3.4))
  log $ show $ fillRadial # preview _centerPoint <#> review _centerPoint ------------------------------------- (Just (RadialGradient rgba 0 0 0 1.0 rgba 255 255 255 1.0 (1.0, 3.4))) -- ✗ (breaks preview-review law)
  log $ show $ fillRadial # preview _centerPoint' <#> review _centerPoint' ----------------------------------- (Just (RadialGradient rgba 255 255 255 1.0 rgba 0 0 0 1.0 (1.0, 3.4))) -- ✓
  log $ show $ preview (_Right <<< _solidFill) (Right $ Solid Color.white) ----------------------------------- (Just rgba 255 255 255 1.0)
  log $ show $ preview (_Right <<< _solidFill) (Left $ Solid Color.white) ------------------------------------ Nothing
  log $ show $ preview (_Right <<< _solidFill) (Right NoFill) ------------------------------------------------ Nothing
  log $ show $ (review (_Right <<< _solidFill) Color.white :: Either Unit Fill) ------------------------------ (Right (Solid rgba 255 255 255 1.0)) -- uses both constructors
  log $ show $ preview (traversed <<< _solidFill) [ Solid Color.white, Solid Color.black ] ------------------- (Just rgba 255 255 255 1.0)
  log $ show $ toListOf (traversed <<< _solidFill) [ Solid Color.white, Solid Color.black ] ------------------ (rgba 255 255 255 1.0 : rgba 0 0 0 1.0 : Nil)
  log $ show $ preview (_Right <<< traversed) (Right [ 1, 2, 3 ]) -------------------------------------------- (Just 1)
  log $ show $ ((set (_Right <<< traversed) 888 (Right [ 1, 2, 3 ])) :: Either Unit (Array Int)) ------------- (Right [888,888,888])
  log $ show $ ((set (_Right <<< traversed) 888 (Left 33)) :: Either Int (Array Int)) ------------------------ (Left 33)
  log $ show $ preview (_1 <<< _solidFill) $ Tuple (Solid Color.white) "ignore" ------------------------------ (Just rgba 255 255 255 1.0)
  log $ show $ preview (_1 <<< _solidFill) $ Tuple NoFill "ignore" ------------------------------------------- Nothing
  log $ show $ preview _Just (Just 3) ------------------------------------------------------------------------ (Just 3)
  log $ show $ review _Just 3 -------------------------------------------------------------------------------- (Just 3)
  log $ show $ (is _Just (Just 3) :: Boolean) ---------------------------------------------------------------- true
  log $ show $ "3838" # review _intSource # preview _intSource ----------------------------------------------- (Just "3838") -- review-preview law
  log $ show $ "3838" # preview _intSource <#> review _intSource --------------------------------------------- (Just "3838") -- preview-review law
  log $ show $ 3838 # review _int # preview _int ------------------------------------------------------------- (Just 3838)
  log $ show $ "3838" # preview _int <#> review _int --------------------------------------------------------- (Just "3838")
