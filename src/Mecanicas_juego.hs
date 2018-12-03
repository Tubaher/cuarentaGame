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
botarCarta card gm@(Game j1@(Jugador _ mano1 _) j2@(Jugador _ mano2 _) board tn act _)
            | tn == 1 = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador1 = j1{cartasMano = filter (\x -> x /= card) mano1 }, action = 0}
            | otherwise = gm{mesaDeJuego = (card, ultimoIndice(board)) : board, jugador2 = j2{cartasMano = filter (\x -> x /= card) mano2 }, action = 0}
        

equals :: Mesa -> Bool
equals (x@(cart1,ind1):y@(cart2,ind2):xs) 
        | ind1 -1 == ind2 =  snd(cart1) == snd(cart2)
        | otherwise = False

checkCaida :: Game -> Game
checkCaida gm@(Game j1@(Jugador crt1 _ pts1) j2@(Jugador crt2 _ pts2) board@(x:y:xs) tn act _) 
            | tn == 1 = if equals(board) then gm{jugador1 = j1{carton = crt1 +2 , puntos = pts1 +2},mesaDeJuego = xs, action = 1} else gm
            | otherwise = if equals(board) then gm{jugador2 = j2{carton = crt2 +2 , puntos = pts2 +2},mesaDeJuego = xs,  action = 1} else gm

-----------------sumas------------
--devuelve una lista de listas de 2 elementos que son cartas que pueden generar la suma de un elemento
sumas :: CartaEnMesa -> Mesa -> [[CartaEnMesa]]
sumas _ [] = []
sumas c@(card, ind) mesa = quitarReciprocos([[x,y] | x<-mesa, y<-mesa, fst(x) /= fst(y), snd(fst(x)) + snd(fst(y)) == snd(card), snd(fst(x)) + snd(fst(y)) <= 7])
        where   quitarReciprocos [] = []
                quitarReciprocos l@(x:xs) = x : quitarReciprocos(filter (\duo -> duo /= reverse(x)) xs)  

                      
encontrarCartaIdentica :: CartaEnMesa -> Mesa -> Integer
encontrarCartaIdentica _ [] = 0
encontrarCartaIdentica ((_,val),ind) board2
            |  filter(\((_,num),_) -> num == val) board2 == [] = 0
            | otherwise = snd(head(filter(\((_,num),_) -> num == val) board2)) 

encontrarCartaSuma :: CartaEnMesa -> Mesa -> [Integer]
encontrarCartaSuma _ [] = []
encontrarCartaSuma cardMesa board
            | length(board) == 1 = []
            | otherwise = if sumas cardMesa board == [] then [] else map (`encontrarCartaIdentica` board)  (head(sumas cardMesa board))  

indicesEscalera :: CartaEnMesa -> Mesa -> [Integer]
indicesEscalera _ [] = []
indicesEscalera cm@(card@(s,val),ind) m
            | val <7 = snd(head(if filter (\((_,valueI), _) -> succ val == valueI) m == [] then [(card,0)] else filter (\((_,valueI), _) -> succ val == valueI) m)) : indicesEscalera ((s,succ val), ind) m
            | (val >= 7) && (val < 10)= snd(head(if filter (\((_,valueI), _) -> succ val == valueI) m == [] then [(card,0)] else filter (\((_,valueI), _) -> (succ val) +3 == valueI) m)) : indicesEscalera ((s,succ val), ind) m
            |otherwise = []

eliminarCartaMesaInd :: Mesa -> Integer -> Mesa 
eliminarCartaMesaInd [] _ = []
eliminarCartaMesaInd m ind = filter (\(_,val) -> val/= ind) m 

llevarCartonEscalera :: Game -> Game 
llevarCartonEscalera gm@(Game j1@(Jugador crt1 _ _) j2@(Jugador crt2 _ _) board@(x:xs) tn _ _)
            | tn == 1 = case aplicaPrimero x xs of 0 -> gm
                                                   1 -> gm{mesaDeJuego = foldl eliminarCartaMesaInd xs (quitarZeros(indicesEscalera x xs) ++ encontrarCartaSuma x xs) ,jugador1 = j1{ carton = toInteger(length(quitarZeros(indicesEscalera x xs) ++ encontrarCartaSuma x xs)) + crt1 + 1} }   --Eliminar cartaMesa debe coger una Mesa y carta
                                                   2 -> gm{mesaDeJuego = foldl eliminarCartaMesaInd xs (quitarZeros(indicesEscalera x xs) ++ (encontrarCartaIdentica x xs) : [] ), jugador1 = j1{ carton = toInteger(length(quitarZeros(indicesEscalera x xs) ++ (encontrarCartaIdentica x xs) : [] )) + crt1 + 1} }
            | otherwise = case aplicaPrimero x xs of 0 -> gm
                                                     1 -> gm{mesaDeJuego = foldl eliminarCartaMesaInd xs (quitarZeros(indicesEscalera x xs) ++ encontrarCartaSuma x xs) , jugador2 = j2{ carton = toInteger(length(quitarZeros(indicesEscalera x xs) ++ encontrarCartaSuma x xs)) + crt2 + 1} }   --Eliminar cartaMesa debe coger una Mesa y carta
                                                     2 -> gm{mesaDeJuego = foldl eliminarCartaMesaInd xs (quitarZeros(indicesEscalera x xs) ++ (encontrarCartaIdentica x xs) : [] ), jugador2 = j2{ carton = toInteger(length(quitarZeros(indicesEscalera x xs) ++ (encontrarCartaIdentica x xs) : [] )) + crt2 + 1} }
            where aplicaPrimero card tablero 
                        | encontrarCartaSuma x xs == [] = if encontrarCartaIdentica x xs == 0 then 3 else 2
                        | otherwise = 1
                  quitarZeros l = filter (\ele -> ele /= 0) l


limpiaMesa :: Game -> Game
limpiaMesa gm@(Game _ _  board tn act _)
    | act == 1 = gm
    | otherwise = actTo1(case vaciaMesayn(llevarCartonEscalera gm) of True -> masDos(llevarCartonEscalera(gm))
                                                                      False -> llevarCartonEscalera(gm))
    where  vaciaMesayn game@(Game _ _ board _ _ _) = board == []
           masDos game@(Game j1@(Jugador _ _ puntos1) j2@(Jugador _ _ puntos2) _ turno _ _) 
                | turno == 1 = game{jugador1 = j1{puntos = puntos1 +2}}
                | otherwise = game{jugador2 = j2{puntos = puntos2 +2}}
           actTo1 gameTmp@(Game _ _ _ _ _ _) =  gameTmp{action = 1}


