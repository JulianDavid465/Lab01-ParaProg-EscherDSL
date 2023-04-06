module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where
import Dibujo

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.

cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pre fun dibu = foldDib (\lmnt -> applyfun lmnt) id id id 
                                                  (\f1 f2 d1 d2 -> apilar f1 f2 (cambiar pre fun d1) (cambiar pre fun d2))
                                                  (\f1 f2 d1 d2 -> juntar f1 f2 (cambiar pre fun d1) (cambiar pre fun d2))
                                                  (\d1 d2 -> encimar (cambiar pre fun d1) (cambiar pre fun d2))
                                                  dibu
                                  where applyfun lmnt = case pre lmnt of
                                                          True -> fun lmnt
                                                          False ->figura lmnt

-- Alguna básica satisface el predicado.
anyDib = undefined

-- Todas las básicas satisfacen el predicado.
allDib = undefined

-- Los dos predicados se cumplen para el elemento recibido.
andP = undefined

-- Algún predicado se cumple para el elemento recibido.
orP = undefined
