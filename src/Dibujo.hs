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


data Dibujo a = Figura a
                |Rotar (Dibujo a) 
                |Espejar (Dibujo a)
                |Rot45 (Dibujo a)
                |Apilar Float Float (Dibujo a) (Dibujo a)
                |Juntar Float Float (Dibujo a) (Dibujo a)
                |Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

comp :: (a -> a) -> Int -> a -> a
comp f 0 dibu = dibu
comp f num dibu = case num>=0 of
                    True -> f(comp f (num-1) dibu)
                    False-> error "No se puede componer negativas veces"
-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura lmnt = Figura lmnt

rotar :: Dibujo a -> Dibujo a
rotar dibu = Rotar dibu

espejar :: Dibujo a -> Dibujo a
espejar dibu = Espejar dibu

rot45 :: Dibujo a -> Dibujo a
rot45 dibu = Rot45 dibu

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar f1 f2 dibu1 dibu2 = Apilar f1 f2 dibu1 dibu2

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar f1 f2 dibu1 dibu2 = Juntar f1 f2 dibu1 dibu2

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar dibu1 dibu2 = Encimar dibu1 dibu2

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 dibu = comp Rotar 2 dibu

r270 :: Dibujo a -> Dibujo a
r270 dibu = comp Rotar 3 dibu

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) dibu1 dibu2 = Apilar 1.0 1.0 dibu1 dibu2

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) dibu1 dibu2 = Juntar 1.0 1.0 dibu1 dibu2

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) dibu1 dibu2 = Encimar dibu1 dibu2

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = (.-.) ((///) d1 d2) ((///) d3 d4)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dibu = (^^^) ((^^^) (dibu) (Rotar dibu)) ((^^^) (r180 dibu) (r270 dibu))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dibu = cuarteto (dibu) (Rotar dibu) (r180 dibu) (r270 dibu)

-- Estructura general para la semántica (a no asustarse). Ayuda: 
-- pensar en foldr y las definiciones de Floatro a la lógica
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b

foldDib fig rot esp r45 api jun enc dibu = case dibu of
                                             Figura  lmnt  ->             fig lmnt
                                             Rotar   dibu2  ->            rot (foldDib fig rot esp r45 api jun enc dibu2)
                                             Espejar dibu2->              esp (foldDib fig rot esp r45 api jun enc dibu2)
                                             Rot45   dibu2  ->            r45 (foldDib fig rot esp r45 api jun enc dibu2)
                                             Apilar  f1 f2 dibu1 dibu2 -> api  f1 f2 (foldDib fig rot esp r45 api jun enc dibu1) (foldDib fig rot esp r45 api jun enc dibu2)
                                             Juntar  f1 f2 dibu1 dibu2 -> jun  f1 f2 (foldDib fig rot esp r45 api jun enc dibu1) (foldDib fig rot esp r45 api jun enc dibu2)
                                             Encimar dibu1 dibu2 ->       enc (foldDib fig rot esp r45 api jun enc dibu1)        (foldDib fig rot esp r45 api jun enc dibu2)


-- foldDib figur rotar espejar rotar45 apilar juntar encimar (Encimar (Figura cuadrado) (Figura triangulo))


-- Demostrar que `mapDib figura = id`
mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f dibu = case dibu of
                    Figura  dibu2 ->               f dibu2
                    Rotar   dibu2 ->               Rotar (mapDib f dibu2)
                    Espejar dibu2 ->               Espejar (mapDib f dibu2)
                    Rot45   dibu2 ->               Rot45 (mapDib f dibu2)
                    Apilar  f1 f2 dibu1 dibu2   -> Apilar f1 f2 (mapDib f dibu1) (mapDib f dibu2)
                    Juntar  f1 f2 dibu1 dibu2   -> Juntar f1 f2 (mapDib f dibu1) (mapDib f dibu2)
                    Encimar dibu1 dibu2 ->         Encimar (mapDib f dibu1) (mapDib f dibu2)

-- Junta todas las figuras básicas de un dibujo.
{-figuras :: Dibujo a  -> [a]
figuras dibu = case dibu of
                Figura   dibu2 ->                [dibu2]
                Rotar    dibu2 ->                figuras dibu2
                Espejar  dibu2 ->                figuras dibu2
                Rot45    dibu2 ->                figuras dibu2
                Apilar   f1 f2 dibu1 dibu2   -> (figuras dibu1) ++ (figuras dibu2)
                Juntar   f1 f2 dibu1 dibu2   -> (figuras dibu1) ++ (figuras dibu2)
                Encimar  dibu1 dibu2 ->         (figuras dibu1) ++ (figuras dibu2)
-}
-- Version de figuras con foldDib
figuras :: Dibujo a -> [a]
figuras dibu = foldDib (\x -> [x]) id id id (\f1 f2 xs ys ->  xs ++ ys) 
                                             (\f1 f2 xs ys ->  xs ++ ys) 
                                             (\xs ys    ->     xs ++ ys)
                                             dibu
-- (Encimar (Figura cuadrado) (Figura triangulo))
-- (Figura cuadrado)