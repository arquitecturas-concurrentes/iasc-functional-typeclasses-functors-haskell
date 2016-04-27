class GilTypeC t where gil :: j x -> t x j
data Cajita a b = Cajita {estadoCajita :: b a} deriving (Show)
instance GilTypeC Cajita where gil x = Cajita x
