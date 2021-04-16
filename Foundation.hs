module Foundation where

data Sense = Sense String

data Obs = None | Obs String Obs deriving Eq

data Change = End | Change Obs Change | To Obs Change deriving Eq
