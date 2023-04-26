module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo, foldDib)
import FloatingPic (FloatingPic, Output, grid)
--Yo añadí el import de V
import qualified Graphics.Gloss.Data.Point.Arithmetic as V


-- Interpretación de un dibujo
-- formulas sacadas del enunciado
-- type Output a = a -> Vector -> Vector -> Vector -> Picture
interp :: Output a -> Output (Dibujo a)
interp out aa x w h dib      = foldDib intFig intRot intEsp intR45 intApi intJun intEnc (dib aa)
                where dib aa = dibu
                      intFig = (\y -> pictures [out y x w h])
                      intRot = (\y -> out y (x V.+ w) h (-1 V.* w))
                      intEsp = (\y -> out y (x V.+ w) (-1 V.* w) h)
                      intR45 = (\y -> out dibu (y V.+ ( 0.5 V.* (w V.+ h))) ( 0.5 V.* (w V.+ h)) (0.5 V.* (h V.- w)))
                      intApi = (\f1 f2 dibu1 dibu2 -> pictures (out dibu1 (x V.+ h') w (r V.* h) ++ out dibu2 x w h'))
                                                            where r'= f1/(f2+f1)
                                                                  r = f2/(f2+f1)
                                                                  h'= r' V.* h 
                      intJun = (\f1 f2 dibu1 dibu2 -> pictures (out dibu1 x w' h ++ out dibu2 (x V.+ w') (r' V.* w) h))
                                                            where r'= f1/(f2+f1)
                                                                  r = f2/(f2+f1)
                                                                  w'= r' V.* w
                      intEnc = (\dibu1 dibu2 -> pictures (out dibu1 x w h ++ out dibu2 x w h))

--              foldDib intFig intRot intEsp intR45 intApi intJun intEnc (Encimar (Figura cuadrado) (Figura triangulo))
-- Configuración de la interpretación
data Conf = Conf {
        name :: String,
        pic :: FloatingPic
    }

interpConf :: Conf -> Float -> Float -> Picture 
interpConf (Conf _ p) x y = p (0, 0) (x,0) (0,y)

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial cfg size = do
    let n = name cfg
        win = InWindow n (ceiling size, ceiling size) (0, 0)
    display win white $ withGrid (interpConf cfg size size) size size
  where withGrid p x y = translate (-size/2) (-size/2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
        grey = makeColorI 120 120 120 120