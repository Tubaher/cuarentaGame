module Mecanicas_juego where

import Data.List
import Elementos_juego
import Jugadores

{-

verificarRonda :: Mano -> Bool
verificarRonda (Mano{carta1=(x1,y1), carta2=(x2,y2), carta3=(x3,y3), carta4=(x4,y4), carta=(x5,y5) })
    | 

-}
{-
verificarTriunfoInicial :: Mano -> Bool
verificarTriunfoInicial (Mano{carta1=(x1,y1), carta2=(x2,y2), carta3=(x3,y3), carta4=(x4,y4), carta=(x5,y5) }) = if (y1==y2==y3==y4 || y1==y2==y3==y5 || y2==y3==y4==y5 ||  )

-}

listaIndices :: Mano -> [Integer]
listaIndices (Mano{ carta1 = a, carta2 = b, carta3 = c, carta4 =  d, carta5 = e}) = map snd [a,b,c,d,e]

{-}
verificarTriunfoInicial :: [Integer] -> Bool
verificarTriunfoInicial [a,b,c,d,e,f]
    |a==b && a==c && a==d = True
    |b==c && b==d && b==e = True        
    |a==c && a==d && a==e = True
    |a==b && a==d && a==e = True
    |a==b && a==c && a==e = True
    |otherwise  = False
-}

{-
verificarTriunfoInicial :: [Integer] -> Bool 
verificarTriunfoInicial xs  = isNothing(find (\ t -> snd(t) == 4) ((map (\xs -> (head xs, length xs)) . group . sort) xs ))
                            where isNothing Nothing = False
                                  isNothing x = True
-}
    {-| map (\xs -> length (group(sort xs)) /=4 || map (\xs -> length (group(sort xs)) /=3 = False
    |otherwise = True-}




verificarTriunfoInicial :: [Integer] -> Bool
verificarTriunfoInicial xs  
    | True `elem` map (\ws -> (length ws) ==4) (group (sort xs)) = True
    | otherwise = False
     
verificarRonda :: [Integer] -> Bool
verificarRonda xs  
    | True `elem` map (\ws -> (length ws) ==3) (group (sort xs)) = True
    | otherwise = False
     






   {-  | map (\ws -> (length ws) ==4) (group (sort xs)) = True
    | otherwise = False
     -}
    
    {-
    | map (length) (group(sort xs)) == 4 || map (length) (group(sort xs)) ==3  = True
    | map (length) (group(sort xs)) <3 = False
    | map (length) (group(sort xs)) >4 = False
    -}


