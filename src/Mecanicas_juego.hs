module Mecanicas_juego where

import Elementos_juego
import qualified Data.Maybe as Maybe
import qualified Data.List as List 

----------------- listaIndices-------------------
--Funcion que me permite obtener todos los indices de cada Carta de la Mano y almacenarlos en una lista


{- listaIndices :: Mano -> [Integer]
listaIndices (Mano{ carta1 = a, carta2 = b, carta3 = c, carta4 =  d, carta5 = e}) = map snd [a,b,c,d,e] -}

----------------- verificarTriunfoInicial-------------------
--Funcion que me permite verificar si tengo 4 cartas que tengan el mismo indice, si es asi, me devuelve True, en otro caso False


verificarTriunfoInicial :: [Integer] -> Bool
verificarTriunfoInicial xs  
    | True `List.elem` map (\ws -> (length ws) ==4) (List.group (List.sort xs)) = True
    | otherwise = False
     

----------------- verificarRonda-------------------
--Funcion que me permite verificar si tengo 3 cartas con los mismos indices, si es asi, me devuelve True, en otro caso False


verificarRonda :: [Integer] -> Bool
verificarRonda xs  
    | True `List.elem` map (\ws -> (length ws) ==3) (List.group (List.sort xs)) = True
    | otherwise = False

----------------- ultimoIndice-------------------
--Funcion que me permite obtener el indice de la ultima carta que ingresa a la mesa

ultimoIndice :: Mesa -> Integer
ultimoIndice l = foldl (\acc (_,numero) -> if acc>numero then acc else numero) 0 l

----------------- ultimoIndiceElem-------------------
--Funcion que me devuelve la carta que corresponde al ultimo indice

ultimoIndiceElem :: Mesa -> Carta
ultimoIndiceElem mesa = fst(Maybe.fromMaybe (("brillo",1),-1) (List.find (\(_, indice) -> indice == ultimoIndice mesa) mesa ))

----------------- botarCartaMesa-------------------   
--Esta función recibe una carta y la agrega a la mesa con un contador para tener en cuenta en que orden entró

botarCartaMesa :: Carta -> Mesa -> Mesa 
botarCartaMesa c m = (c, ultimoIndice(m)+1) : m 

----------------- botarCarta-------------------
--Funcion que me permite botar una carta del jugador a la mesa
--sin que acabe su turno

botarCarta :: Carta -> Game -> Game 
<<<<<<< HEAD
botarCarta cart gm =  gm

acumularCarton :: Carta -> [Carta]
acumularCarton 

acumularCarton :: Game -> Game
acumularCarton juego
    |juego{ snd (fst (last(mesaDeJuego)))}



acumularCarton juego{jugador1={carton= _, cartasMano= _, puntos= _}, jugador2={carton= _, cartasMano= _, puntos= _}, mesaDeJuego= _, turno= _, }
=======
botarCarta card gm@(Game j1@(Jugador _ mano1 _) j2@(Jugador _ mano2 _) board tn)
            | tn == 1 = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador1 = j1{cartasMano = filter (\x -> x /= card) mano1 }}
            | otherwise = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador2 = j2{cartasMano = filter (\x -> x /= card) mano2 }}


>>>>>>> 54f92f3128b037fc585f083d8d7407d2cf7ccdb7
 