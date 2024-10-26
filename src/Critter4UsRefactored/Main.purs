module Critter4UsRefactored.Main
  ( module Model
  , module TagDb
  , Action(..)
  , update
  ) where

import Critter4Us.TagDb (TagDb, idsFor, tagsFor) as TagDb
import Critter4UsRefactored.Animal as Animal
import Critter4UsRefactored.Model (Model)
import Critter4UsRefactored.Model (Model, addAnimal, addAnimalTag, initialModel) as Model

data Action
  = AddAnimal Animal.Id String
  | AddTag Animal.Id String

update ∷ Model → Action → Model
update model (AddAnimal animalId name) = Model.addAnimal animalId name model
update model (AddTag animalId tag) = Model.addAnimalTag animalId tag model
