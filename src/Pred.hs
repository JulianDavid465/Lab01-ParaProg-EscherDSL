module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pre fun dibu = mapDib fun dibu

cambiarPb :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiarPb pre fun dibu = foldDib (applyfun lmnt) id id id 
                                                  (apilar f1 f2 (cambiarPb pre fun d1) (cambiarPb pre fun d2))
                                                  (juntar f1 f2 (cambiarPb pre fun d1) (cambiarPb pre fun d2))
                                                  (encimar (cambiarPb pre fun d1) (cambiarPb pre fun d2))
                                  where applyfun lmnt = case pre lmnt of
                                                          true = fun lmnt
                                                          false= id

-- Alguna básica satisface el predicado.
anyDib = undefined

-- Todas las básicas satisfacen el predicado.
allDib = undefined

-- Los dos predicados se cumplen para el elemento recibido.
andP = undefined

-- Algún predicado se cumple para el elemento recibido.
orP = undefined
