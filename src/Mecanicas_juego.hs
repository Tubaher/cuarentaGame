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

----------------- botarCarta-------------------
--Toma una carta y un Game para realizar la accion de botar una carta en 
--el tablero de juego que consiste en: agregar la carta en Game-MesaDeJuego y 
--barrarla de la mano del jugador que la botó

botarCarta :: Carta -> Game -> Game 
botarCarta card gm@(Game j1@(Jugador _ mano1 _) j2@(Jugador _ mano2 _) board tn act)
            | tn == 1 = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador1 = j1{cartasMano = filter (\x -> x /= card) mano1 }, action = 0}
            | otherwise = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador2 = j2{cartasMano = filter (\x -> x /= card) mano2 }, action = 0}
        

equals :: Mesa -> Bool
equals (x@(cart1,ind1):y@(cart2,ind2):xs) 
        | ind1 -1 == ind2 =  snd(cart1) == snd(cart2)
        | otherwise = False

checkCaida :: Game -> Game
checkCaida gm@(Game j1@(Jugador crt1 _ pts1) j2@(Jugador crt2 _ pts2) board@(x:y:xs) tn act) 
            | tn == 1 = if equals(board) then gm{jugador1 = j1{carton = crt1 +2 , puntos = pts1 +2},mesaDeJuego = xs, action = 1} else gm
            | otherwise = if equals(board) then gm{jugador2 = j2{carton = crt2 +2 , puntos = pts2 +2},mesaDeJuego = xs,  action = 1} else gm