module Main where

import Graphics.Gloss
import FloatingPic
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- el vector nulo
cero :: Vector
cero = (0, 0)

half :: Vector -> Vector
half = (0.5 V.*)

add :: Vector -> Vector -> Vector
add v1 v2 = v1 V.+ v2

data TriORect = Triangulo | Rectangulo deriving (Eq, Show)

poner :: Output TriORect
poner lmnt _ _ _ = case lmnt of 
                 Triangulo -> translate (0) (0) $ color red $ polygon [(0, 0), (50, 100), (100, 0)]
                 Rectangulo-> translate (0) (0) $ color red $ polygon [(0, 0), (50, 0), (50, 50), (0, 50)]

main :: IO ()
main = display (InWindow "My window" (400, 400) (0, 0)) white (poner Rectangulo cero cero cero)

--main :: IO ()
--main = display (InWindow "My window" (400, 400) (0, 0)) white (pictures [Circle 30, pictures[Circle 40]])