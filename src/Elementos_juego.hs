module Elementos_juego where
 
data Jugador = Jugador {
    turno :: Integer,
    cartasMano :: Mano,
    puntos :: Integer
}
{-data Carta = Carta{ numero :: Integer
                  , palo :: String
                  }
-} --No funciona en la funcion creandoCarta, solo funciona con tuplas
  
type Baraja = [Carta]

type Carta = (String, Integer)

data Mano = Mano { carta1 :: Carta
                 , carta2 :: Carta
                 , carta3 :: Carta
                 , carta4 :: Carta
                 , carta5 :: Carta
                 } deriving (Show)



creandoBaraja :: Baraja
creandoBaraja = [carta | x <- ["Brilo", "Corazon", "Trebol", "Negro"], y <- [1,2,3,4,5,6,7,11,12,13], let carta= (x,y)] --OJO: quitar 8 9 10
 


type Mesa = [(Carta, Integer)]      

ultimoIndice :: Mesa -> Integer
ultimoIndice l = foldl (\acc (_,numero) -> if acc>numero then acc else numero) 0 l

--Esta función resibe una carta y la agrega a la mesa con un contador para tener en cuenta en que orden entró
botarCartaMesa :: Carta -> Mesa -> Mesa 
botarCartaMesa c m = (c, ultimoIndice(m)+1) : m 
 
-- solo realiza la jugada de caida y agrega puntos no retira la carta de la mesa
caidaMesa :: Carta-> Jugador -> Mesa -> Jugador 
caidaMesa c jugador{turno = tur, cartasMano = mano, puntos= pts} mesa 
        | 
--addBaraja :: Carta -> [Carta] -> [Carta]
--addBaraja carta x = carta : x
--addBaraja (x:xs)

{- addBaraja :: Carta -> Baraja -> Baraja
addBaraja x baraja = takeWhile((length baraja) <= 53) filter(isInBaraja x)baraja -}
