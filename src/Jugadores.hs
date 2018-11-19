module Jugadores where

import Elementos_juego


data Jugador = Jugador {
    turno :: Integer,
    cartasMano :: Mano,
    puntos :: Integer
}



