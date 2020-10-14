module Slot where

data SlotColor = Black | White deriving (Enum, Eq, Show)
data Slot = Slot SlotColor Int 

class ISlot s where
    getColor :: s -> SlotColor
    getValue :: s -> Int

instance ISlot Slot where

    getColor (Slot color _) = color
    getValue (Slot _ value) = value

instance Eq Slot where
    x == y = (((getColor x) == (getColor y)) && ((getValue x) == (getValue y)))

instance Ord Slot where
    (Slot _ v1) `compare` (Slot _ v2) = v1 `compare` v2

instance Show Slot where
    show (Slot color value) = "(" ++ show color ++ "," ++ show value ++ ")" 