module Elementos_juego where

import Data.Maybe
import qualified Data.List as List 


--Tipos de datos que se userá en el juego

-----------Jugador------------
-- Lleva cuenta de la cantidad de cartas que el jugador ha levantado
--también gurda las cartas que el jugador tiene en mano
--y cuenta los puntos que el jugador gana
data Jugador = Jugador {
    carton :: Integer,
    cartasMano :: Baraja,
    puntos :: Integer
    } deriving (Show)

----------Mano--------------
--En esta estructura linkeamos las cartas de la mano del jugador
{- data Mano = Mano { 
    carta1 :: Carta,
    carta2 :: Carta,
    carta3 :: Carta,
    carta4 :: Carta,
    carta5 :: Carta
    } deriving (Show) -}

----------Game-------------
--En game linkeamos todos los datos que varian durante el juego
--primero creamos dos jugadores que seran quienes compitan en la
-- partida, y agregamos una mesa de juego junto con un contador
-- que varía dependiendo de quien es el turno.
data Game = Game {  
    jugador1 :: Jugador,
    jugador2 :: Jugador,
    mesaDeJuego :: Mesa,
    turno :: Integer,
    action :: Integer,
    mazoDeCartas :: Baraja 
    } deriving (Show)  

{-data Carta = Carta{ numero :: Integer
                  , palo :: String
                  }
-} --No funciona en la funcion creandoCarta, solo funciona con tuplas
  

------------ Sinónimos de tipo------------------
-----------Baraja-------------------
--Bara es un sinónimo que usaremos para referirnos a una lista de cartas
type Baraja = [Carta]

-----------Carta-------------------
--Carta es un sinónimo que usaremos para una dupla de String e Interger
type Carta = (String, Integer)

-----------Mesa---------------------
--Mesa es un sinónimo que usaremos para referirnos a una lista de duplas
--que constarán de una Carta y un Interger
--Integer es el indice en el que ha entrado la carta
type Mesa = [(Carta, Integer)] 


----------------- Creando Baraja-------------------
--Función que crea la baraja que usremos en el juego
creandoBaraja :: Baraja
creandoBaraja = [carta | x <- ["Brilo", "Corazon", "Trebol", "Negro"], y <- [1,2,3,4,5,6,7,11,12,13], let carta= (x,y)] --OJO: quitar 8 9 10
 


     



-- solo realiza la jugada de caida y agrega puntos no retira la carta de la mesa
{- caidaMesa :: Carta-> Jugador -> Mesa -> Jugador 
caidaMesa c jugador{turno = tur, cartasMano = mano, puntos= pts} mesa -} 
--addBaraja :: Carta -> [Carta] -> [Carta]
--addBaraja carta x = carta : x
--addBaraja (x:xs)

{- addBaraja :: Carta -> Baraja -> Baraja
addBaraja x baraja = takeWhile((length baraja) <= 53) filter(isInBaraja x)baraja -}
