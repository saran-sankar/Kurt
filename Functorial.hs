module Functorial where

class Interaction f where
  changeF :: (Change -> Change) -> (Change -> Change)
