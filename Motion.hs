module Motion where

import Foundation

data Obj = Obj Obs deriving Eq
-- forming the notion of an object from possible observations

move :: Change -> Change
move (Change x (To None End)) =
  Change None (To x End)
-- this encodes what motion means under a certain interpretation

data MoveObj = NoMove | MoveObj Obj Sense Sense

motion :: Sense -> Sense -> Obj -> Obj -> Obj -> Obj -> MoveObj
motion o s (Obj a) (Obj b) (Obj c) (Obj d) =
  if (b == None) -- && (a /= None)
    && (move((Change a (To b End))) == Change c (To d End))
    then MoveObj (Obj a) o s
    else NoMove

stringToObj "none" = Obj None
stringToObj x      = (Obj (Obs x None))
-- task: edit this

objToString None = ""
objToString (Obs x y) = x ++ "_" ++ objToString (y)

observation :: Sense -> Sense -> [String] -> [String] -> MoveObj
observation (o) (s) (a:b:[]) (c:d:[]) =
  motion o s
  (stringToObj a) (stringToObj b) (stringToObj c) (stringToObj d)

encode :: MoveObj -> String
encode (MoveObj (Obj a) (Sense x) (Sense y)) = objToString (a) ++ "objMove_" ++ x ++ "_" ++ y
encode NoMove = ""
