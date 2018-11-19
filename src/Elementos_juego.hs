module Elementos_juego where

{-data Carta = Carta{ numero :: Integer
                  , palo :: String
                  }
-} --No funciona en la funcion creandoCarta, solo funciona con tuplas
  
type Baraja = [Carta]

type Carta= (String, Integer)

data Mano = Mano { carta1 :: Carta
                 , carta2 :: Carta
                 , carta3 :: Carta
                 , carta4 :: Carta
                 , carta5 :: Carta
                 } deriving (Show)



creandoBaraja :: Baraja
creandoBaraja = [carta | x <- ["Brilo", "Corazon", "Trebol", "Negro"], y <- [1,2,3,4,5,6,7,11,12,13], let carta= (x,y)] --OJO: quitar 8 9 10
 


                        
--addBaraja :: Carta -> [Carta] -> [Carta]
--addBaraja carta x = carta : x
--addBaraja (x:xs)

{- addBaraja :: Carta -> Baraja -> Baraja
addBaraja x baraja = takeWhile((length baraja) <= 53) filter(isInBaraja x)baraja -}






