import Data.List
import Data.List.Split
import Utilities
import Foundation
import Motion
import Temperature

obsObjChange = [Sense "i1", Sense "i2"]
-- observables correspoding to ObjChange

derived_strings :: [MoveObj] -> [String]
derived_strings [] = []
derived_strings (x:xs) = if encode(x) == "" then derived_strings(xs)
  else encode(x):derived_strings(xs)

derMotion (x:y:[]) =
  observation (Sense "i1") (Sense "i2") (words x) (words y) :
  observation (Sense "i2") (Sense "i1") (words y) (words x) : []

derTemp z = NoMove

derivation :: [String] -> [MoveObj]
derivation (x:y:z:[])  =  -- we have three observables
   append (derTemp z) (derMotion (x:y:[]))

main = do
    observation_string <- getLine
    let derived = derivation(endBy ";" observation_string)
    print (derived_strings(derived))
