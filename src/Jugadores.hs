module Jugadores where

import Elementos_juego


data Jugador = Jugador {
    turno :: Integer,
    cartasMano :: Mano,
    puntos :: Integer
}



botarCarta :: (Mano -> Carta) -> Mano -> Carta
botarCarta f mano = f mano --La funcion a aplicar es una funcion
--que me permite elegir una carta






