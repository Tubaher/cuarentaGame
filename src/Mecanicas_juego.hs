module Mecanicas_juego where

import Elementos_juego
import qualified Data.Maybe as Maybe
import qualified Data.List as List 


listaIndices :: Mano -> [Integer]
listaIndices (Mano{ carta1 = a, carta2 = b, carta3 = c, carta4 =  d, carta5 = e}) = map snd [a,b,c,d,e]

verificarTriunfoInicial :: [Integer] -> Bool
verificarTriunfoInicial xs  
    | True `List.elem` map (\ws -> (length ws) ==4) (List.group (List.sort xs)) = True
    | otherwise = False
     
verificarRonda :: [Integer] -> Bool
verificarRonda xs  
    | True `List.elem` map (\ws -> (length ws) ==3) (List.group (List.sort xs)) = True
    | otherwise = False
     
ultimoIndice :: Mesa -> Integer
ultimoIndice l = foldl (\acc (_,numero) -> if acc>numero then acc else numero) 0 l
 
ultimoIndiceElem :: Mesa -> Carta
ultimoIndiceElem mesa = fst(Maybe.fromMaybe (("brillo",1),-1) (List.find (\(_, indice) -> indice == ultimoIndice mesa) mesa ))
    
--Esta función recibe una carta y la agrega a la mesa con un contador para tener en cuenta en que orden entró
botarCartaMesa :: Carta -> Mesa -> Mesa 
botarCartaMesa c m = (c, ultimoIndice(m)+1) : m 
     
botarCarta :: Carta -> Game -> Game 
botarCarta cart gm =  gm