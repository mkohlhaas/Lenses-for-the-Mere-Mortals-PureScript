module Critter4Us.Main
  ( module Model
  , Action(..)
  , update
  ) where

import Critter4Us.Animal as Animal
import Critter4Us.Model (Model, addAnimal, addAnimalTag, initialModel) as Model
import Critter4Us.Model (Model)

data Action
  = AddAnimal Animal.Id String
  | AddTag Animal.Id String

update ∷ Model → Action → Model
update model (AddAnimal animalId name) = Model.addAnimal animalId name model
update model (AddTag animalId tag) = Model.addAnimalTag animalId tag model
