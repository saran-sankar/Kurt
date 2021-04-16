module Utilities where

append a [] = [a]
append a (x:xs) = x : append a xs
