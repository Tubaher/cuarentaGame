module Elementos_juego where

data Carta = Carta{
    numero :: Integer,
    palo :: String 
}
type Baraja = [Carta]

{-creandoCarta ::Integer -> String -> Carta
creandoCarta x y = Carta(x,y) -}

{- creandoCarta :: [String]->[Integer] -> Carta
creandoCarta xs yx = [Carta | (x <- xs, y <- ys, let x=(x,y))]
 -}

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






