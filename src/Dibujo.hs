{-# LANGUAGE LambdaCase #-}
module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib,
    figuras
) where


{-
Gramática de las figuras:
<Fig> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> <Fig> 
    | Juntar <Float> <Float> <Fig> <Fig> 
    | Encimar <Fig> <Fig>
-}


data Dibujo a = Figura Fig
                |Rotar a 
                |Espejar a
                |Rot45 a
                |Apilar Float Float Dibujo a Dibujo a
                |Juntar Float Float Dibujo a Dibujo a
                |Encimar Dibujo a Dibujo a
    deriving (Eq, Show)

comp :: (a -> a) -> Int -> a -> a
comp f 0 dibu = dibu
comp f num dibu = case num>=0 of
                    True -> f(comp f (num-1) dibu)
                    False-> error "No se puede componer negativas veces"
-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura lmnt = Dibujo lmnt

rotar = undefined

espejar = undefined

rot45 = undefined

apilar = undefined

juntar = undefined

encimar = undefined


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 dibu = comp Rotar 2 dibu

r270 :: Dibujo a -> Dibujo a
r270 dibu = comp Rot45 6 dibu

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) dibu1 dibu2 = Apilar 0.5 0.5 dibu1 dibu2

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) dibu1 dibu2 = Juntar 0.5 0.5 dibu1 dibu2

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) dibu1 dibu2 = Encimar dibu1 dibu2

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto = Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = Apilar (Juntar d1 d2) (Juntar d3 d4)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dibu = Encimar (Encimar (Rot45 dibu) (Rot45 dibu)) (Encimar (Rot45 dibu) (Rot45 dibu))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dibu = cuarteto (dibu) (Rot45 dibu) (Rotar dibu) (comp Rot45 3 dibu)

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib = undefined

-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f dibu = 

-- Junta todas las figuras básicas de un dibujo.
figuras = undefined
