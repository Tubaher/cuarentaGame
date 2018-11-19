module Jugador where

data Carta = Carta{
    numero :: Integer,
    palo :: String 
}

data Mano = Mano { carta1 :: Carta
                 , carta2 :: Carta
                 , carta3 :: Carta
                 , carta4 :: Carta
                 , carta5 :: Carta
                 } 

data Player = Player { mano :: Mano }