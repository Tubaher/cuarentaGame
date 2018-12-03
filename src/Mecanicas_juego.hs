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


{-
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _), j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(tail(x)))+ snd(fst(tail(xs)))) (board) || (snd(fst(last(board))) `elem` map(\(xs) -> snd(fst(tail(xs)))) (board) = 
    
    
    snd(fst(last(board))) -}
{-
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst xs))) (tail board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador1 = j1{carton = (+3) contcarton1}}
    | tn==2 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst xs))) (tail board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador1 = j1{carton = (+3) contcarton1}}
    | otherwise gm=gm
-}
{-
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && (snd(fst(last(board))) `elem` map(\(x:xs) a -> snd(fst(x)) + (snd(fst a)) | a <- xs) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador1 = j1{carton = (+3) contcarton1}}
    | tn==2 && (snd(fst(last(board))) `elem` map(\(x:xs) a -> snd(fst(x)) + (snd(fst a)) | a <- xs) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador2 = j2{carton = (+3) contcarton2}}
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=turno}
-}


{- acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst map(\(x:xs) -> x)))) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador1 = j1{carton = (+3) contcarton1}}
    | tn==2 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst map(\(x:xs) -> x)))) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador2 = j2{carton = (+3) contcarton2}}
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=turno}
 -}

sumas :: Carta -> Baraja -> [[Carta]]
sumas c mazo = quitarReciprocos([[x,y] | x<-mazo, y<-mazo, x /= y, snd(x) + snd(y) == snd(c), snd(x) + snd(y) <= 7])
        where   quitarReciprocos [] = []
                quitarReciprocos l@(x:xs) = x : quitarReciprocos(filter (\duo -> duo /= reverse(x)) xs)  

deMesaBaraja :: Mesa -> Baraja
deMesaBaraja [] = []
deMesaBaraja board = map (fst) board
{-
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && sumas(fst(last(board)), map(\x -> fst x) init(board)) /= [] = gm{jugador1 = j1{carton = (+3) contcarton1}, mesaDeJuego= takeWhile(\= head(sumas(fst(last(board)), map(\x -> fst x) init(board)))) fst(board)}
    
    {-(snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst map(\(x:xs) -> x)))) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador1 = j1{carton = (+3) contcarton1}}
    | tn==2 && (snd(fst(last(board))) `elem` map(\(x:xs) -> snd(fst(x)) + (snd(fst map(\(x:xs) -> x)))) (init board)) || (snd(fst(last(board))) `elem` map(\ xs -> snd(fst xs)) (tail board)) = gm{jugador2 = j2{carton = (+3) contcarton2}} -}
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=turno}
-}

{-
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn)
    | tn==1 && length(lista_lista_cartas) /=0 = gm{jugador1 = j1{carton = (+3) contcarton1}, mesaDeJuego= takeWhile(/= c1) map(\(a,b) -> a) (init(board)) && takeWhile(/= c2) map(\(a,b) -> a) (init(board))}
    | tn==2 && length(lista_lista_cartas) /=0 = gm{jugador2 = j2{carton = (+3) contcarton2}, mesaDeJuego= takeWhile(/= c1) map(\(a,b) -> a) (init(board)) && takeWhile(/= c2) map(\(a,b) -> a) (init(board))}
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=tn}
    where lista_lista_cartas = sumas (card) (pack)
          card = (\(a,b) -> a) (last(board))
          pack = map(\(a,b) -> a) (init(board))
          c1 = head(head(lista_lista_cartas))
          c2 = (head(lista_lista_cartas))!!1
-}


{- ACUMULAR CARTON ORIGINAL
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn act)
    | tn==1 && act == 0 && length(lista_lista_cartas) /=0 = gm{jugador1 = j1{carton = (+3) contcarton1}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | tn==2 && act == 0 && length(lista_lista_cartas) /=0 = gm{jugador2 = j2{carton = (+3) contcarton2}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=tn, action=act}
    where lista_lista_cartas = sumas (card) (pack)
          card = (\(a,b) -> a) (last(board))
          pack = map(\(a,b) -> a) (init(board))
          c1 = head(head(lista_lista_cartas))
          c2 = (head(lista_lista_cartas))!!1
-}

--Compuesta
acumularCarton :: Game -> Game
acumularCarton gm@(Game j1@(Jugador contcarton1 _ _) j2@(Jugador contcarton2 _ _) board tn act)
    | tn==1 && act == 0 && length(lista_lista_cartas) /=0 = gm{jugador1 = j1{carton = (+3) contcarton1}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | tn==2 && act == 0 && length(lista_lista_cartas) /=0 = gm{jugador2 = j2{carton = (+3) contcarton2}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=tn, action=act}
    where lista_lista_cartas = sumas (card) (pack)
          card = (\(a,b) -> a) (head(board))
          pack = map(\(a,b) -> a) (tail(board))
          c1 = head(head(lista_lista_cartas))
          c2 = (head(lista_lista_cartas))!!1
          
          
--Compuesta
verificarSiEnMesa :: Carta -> Carta -> Carta -> Mesa -> Mesa
verificarSiEnMesa card1 card2 card3 (x:xs)
    | card1 /= fst x && card2 /= fst x  && card3 /= fst x = x : verificarSiEnMesa card1 card2 card3 xs
    | otherwise = verificar
    where verificar = verificarSiEnMesa card1 card2 card3 xs

--COmpuesta
limpiarMesa :: Game -> Game
limpiarMesa gm@(Game j1@(Jugador contcarton1 _ points1) j2@(Jugador contcarton2 _ points2) board tn act)
    | tn==1 && act==0 && length(lista_lista_cartas) /=0 && length(board) ==3 = gm{jugador1 = j1{carton = (+3) contcarton1, puntos = (+2) points1}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | tn==2 && act==0 && length(lista_lista_cartas) /=0 && length(board) ==3 = gm{jugador2 = j2{carton = (+3) contcarton2, puntos = (+2) points2}, mesaDeJuego= verificarSiEnMesa (c1) (c2) (card) (board), action=1 }
    | otherwise = gm{jugador1=j1{carton=contcarton1}, jugador2=j2{carton=contcarton2}, mesaDeJuego=board, turno=tn, action=act}
    where lista_lista_cartas = sumas (card) (pack)
          card = (\(a,b) -> a) (head(board))
          pack = map(\(a,b) -> a) (tail(board))
          c1 = head(head(lista_lista_cartas))
          c2 = (head(lista_lista_cartas))!!1

{-
cartasSiguientes :: Mesa -> Integer -> Mesa
cartasSiguientes (x:xs) sumador
    | ((\((a,b),c)  -> b) (last(xs)) + sumador == (\((a,b),c)  -> b) x ) = x : cartasSiguientes (xs) (sumador+1)
    | otherwise = call_again
    where call_again = cartasSiguientes xs sumador
-}
    --let prueba=[(("Corazon", 4), 5), (("Corazon", 6), 2), (("Brillo", 5), 2), (("Trebol",7),3), (("Brillo",1),4), (("Brillo",11),6) ]
        --let prueba=[(("Corazon", 4), 5), (("Brillo", 5), 2), (("Trebol",7),3), (("Corazon", 6), 2), (("Brillo",1),4), (("Brillo",11),6) ]
--Esta funcion me devuelve en cada las cartas superiores que me puedo llevar (hasta la carta numero 7) (Integer =1)

{-
cartasSiguientes :: Mesa -> Integer -> Mesa
cartasSiguientes j@(x:[]) sumador = [] 
cartasSiguientes j@(x:xs) sumador
    | ((\((a,b),c)  -> b) (last(xs)) + sumador == (\((a,b),c)  -> b) x ) = x : cartasSiguientes (j) (sumador+1)
    | otherwise = call_again
    where call_again = cartasSiguientes xs sumador
-}

{- comparar:: Integer -> Mesa -> (Carta, Integer)
comparar x (y:ys) = if  x == (\((a,b),c)->b) y then y else comparar x ys

cartasSiguientes :: Mesa -> Mesa
cartasSiguientes board = filter (`elem` lista_nueva) board
    where lista_numeros = map(+ ((\((a,b),c) -> b) (head board))) [1..6]
          lista_nueva= comparar ([x | x<-lista_numeros]) (board) -}
{- 
          checkCaida :: Game -> Game
          checkCaida gm@(Game j1@(Jugador crt1 _ pts1) j2@(Jugador crt2 _ pts2) board@(x:y:xs) tn act) 
                      | tn == 1 = if equals(board) then gm{jugador1 = j1{carton = crt1 +2 , puntos = pts1 +2},mesaDeJuego = xs, action = 1} else gm
                      | otherwise = if equals(board) then gm{jugador2 = j2{carton = crt2 +2 , puntos = pts2 +2},mesaDeJuego = xs,  action = 1} else gm -}

                      
encontrarCartaIdentica :: Carta -> Mesa -> Integer
encontrarCartaIdentica _ [] = 0
encontrarCartaIdentica (palo,ind) board2 = snd(head(filter(\((_,num),_) -> num == ind) board2))

encontrarCartaSuma :: Carta -> Mesa -> [Integer]
encontrarCartaSuma _ [] = []
encontrarCartaSuma card board
            | length(board) == 1 = []
            | otherwise =  map (`encontrarCartaIdentica` board)  (head(sumas card (deMesaBaraja board)))  



llevarCartonEscalera :: Game -> Game 
llevarCartonEscalera gm@(Game j1@(Jugador crt1 _ _) j2@(Jugador crt2 _ _) board@(x:xs) tn _)
            | tn == 1 = case aplicaPrimero x xs of 0 -> gm
                                                   1 -> gm{board = foldl eliminarCartaMesaInd xs (indicesEscalera x xs ++ encontrarCartaSuma x xs) , j1{ carton = length(indicesEscalera x xs : encontrarCartaSuma x xs) + crt1 + 1} }   --Eliminar cartaMesa debe coger una Mesa y carta
                                                   2 -> gm{board = foldl eliminarCartaMesaInd xs (indicesEscalera x xs ++ (encontrarCartaIdentica x xs) : [] ), j1{ carton = length(indicesEscalera x xs ++ (encontrarCartaIdentica x xs) : [] ) + crt1 + 1} }
            | otherwise = case aplicaPrimero x xs of 0 -> gm
                                                     1 -> gm{board = foldl eliminarCartaMesaInd xs (indicesEscalera x xs ++ encontrarCartaSuma x xs) , j2{ carton = length(indicesEscalera x xs : encontrarCartaSuma x xs) + crt2 + 1} }   --Eliminar cartaMesa debe coger una Mesa y carta
                                                     2 -> gm{board = foldl eliminarCartaMesaInd xs (indicesEscalera x xs ++ (encontrarCartaIdentica x xs) : [] ), j2{ carton = length(indicesEscalera x xs ++ (encontrarCartaIdentica x xs) : [] ) + crt2 + 1} }
            where aplicaPrimero card tablero 
                        | encontrarCartaSuma == [] = if encontrarCartaIdentica == 0 then 3 else 2
                        | otherwise = 1



    -- | ((\((a,b),c) -> b) x  + map(\a ->a) [1..6]) `elem` map(\((a,b),c) -> b)xs = x
    -- | map(\a->a)
    -- | map( map(\a ->a) lista_numeros  `elem`) map(\((a,b),c)->b)xs = x
     -- | otherwise =[] 
    

{-
ordenarMesa :: Mesa -> Mesa
ordenarMesa (x: x1: xs)
    | (\((a,b),c -> b)) x1 < ordenarMesa xs = x :

    sort((\((a,b),c)->b) xs)
-}

{-
cartasSiguientes :: Mesa -> Integer -> Mesa
--cartasSiguientes [] sumador = []
cartasSiguientes (x:[]) sumador =[]
cartasSiguientes (x:xs) sumador
    | ((\((a,b),c)  -> b) (x) + sumador == (\((a,b),c)  -> b) x ) = x : cartasSiguientes (xs) (sumador+1)
    | otherwise = call_again
    where call_again = cartasSiguientes xs sumador
-}
    
--Si cartasSiguiente de devuelve una lista cuyo ultimo elemento es la carta 7, ahora invoca a verificar JQK (Integer=4 porque 7+4=11)
{- FUncion original
verificarJQK :: Mesa -> Mesa -> Integer -> Mesa
verificarJQK (x:xs) lista_con7 sumador
    | (((\((a,b),c)  -> b) (last(lista_con7)) ) == 7) && (((\((a,b),c)  -> b) (last(lista_con7)) + sumador == (\((a,b),c)  -> b) x )) && (sumador<=6) = x : verificarJQK (xs) (lista_con7) (sumador+1)
    |otherwise = call_again1 
    where call_again1 = verificarJQK  xs lista_con7 sumador
-}

