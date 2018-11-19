module Elementos_juego where

{-data Carta = Carta{ numero :: Integer
                  , palo :: String
                  }
-} --No funciona en la funcion creandoCarta, solo funciona con tuplas
  
type Baraja = [Carta]

type Carta= (String, Integer)

creandoCarta :: [String]->[Integer] -> Baraja
creandoCarta xs yx = [carta | x <- ["Brilo", "Corazon", "Trebol", "Negro"], y <- [1..13], let carta= (x,y)]
 

{- isInBaraja :: Carta -> Baraja -> Bool
isInBaraja carta bara = filter (isOrNot) bara 
            where isOrNot x  
                        | carta == x = True
                        | carta /= x = False -}
                        
--addBaraja :: Carta -> [Carta] -> [Carta]
--addBaraja carta x = carta : x
--addBaraja (x:xs)

{- addBaraja :: Carta -> Baraja -> Baraja
addBaraja x baraja = takeWhile((length baraja) <= 53) filter(isInBaraja x)baraja -}

data Mano = Mano { carta1 :: Carta
                 , carta2 :: Carta
                 , carta3 :: Carta
                 , carta4 :: Carta
                 , carta5 :: Carta
                 } 






