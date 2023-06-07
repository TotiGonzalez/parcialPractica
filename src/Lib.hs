module Lib
    ( someFunc
    ) where
import Unsafe.Coerce (unsafeEqualityProof)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Auto = Auto {
    color :: Color,
    velocidad :: Int,
    distancia :: Int
}deriving Show

data Color =  Rojo | Azul | Blanco | Negro deriving (Show,Eq) 

type Carrera = [Auto]

autoEstaCerca :: Auto -> Auto -> Bool 
autoEstaCerca autoA autoB = (color autoA /= color autoB) && (abs(distancia autoA - distancia autoB) <=10)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = (not . any (autoEstaCerca unAuto) $ unaCarrera) && all ((>) (distancia unAuto) . distancia) unaCarrera

enQuePuesto :: Auto -> Carrera -> Int
enQuePuesto unAuto = (+1) . length . filter ((>) (distancia unAuto) . distancia)

autoCorra :: Int -> Auto -> Auto
autoCorra tiempo unAuto = Auto {distancia = distancia unAuto + velocidad unAuto * tiempo}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad f unAuto = Auto {velocidad = (f . velocidad) unAuto} 

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantBajar unAuto 
 | (velocidad unAuto - cantBajar) < 0 = Auto {velocidad = 0}
 | otherwise = Auto {velocidad = velocidad unAuto - cantBajar}

type PowerUp = Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> PowerUp
terremoto unAuto unaCarrera = afectarALosQueCumplen (autoEstaCerca unAuto) (bajarVelocidad (50)) unaCarrera

miguelitos :: Int -> Auto -> PowerUp
miguelitos cantIndicada unAuto unaCarrera = afectarALosQueCumplen ((>) (distancia unAuto) . distancia) (bajarVelocidad (cantIndicada)) unaCarrera

jetpack :: Int -> Auto -> PowerUp
jetpack cantTiempo unAuto = afectarALosQueCumplen ((==) (color unAuto) . color) (activarJetpack cantTiempo)
activarJetpack :: Int -> Auto -> Auto
activarJetpack tiempo auto = Auto {distancia = distancia auto + velocidad auto * 2 * tiempo}











