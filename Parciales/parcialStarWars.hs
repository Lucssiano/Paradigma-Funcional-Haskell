import Text.Show.Functions ()

{- Punto 1 -}
type Poder = Nave -> Nave

data Nave = UnaNave
  { nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
  }
  deriving (Show)

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 movimientoTurbo

movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque 25

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionDeEmergencia

reparacionDeEmergencia :: Poder
reparacionDeEmergencia = modificarDurabilidad 50 . modificarAtaque (-30)

naveDarthVader :: Nave
naveDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 movimientoSuperTurbo

movimientoSuperTurbo :: Poder
movimientoSuperTurbo = modificarDurabilidad (-45) . repetirPoderNVeces 3 movimientoTurbo

repetirPoderNVeces :: Int -> Poder -> Poder
repetirPoderNVeces n poder nave
  | n < 1 = error "Se debe ingresar un numero mayor a 0"
  | n == 1 = poder nave
  | otherwise = repetirPoderNVeces (n - 1) poder (poder nave) -- ver bien esto

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 (reparacionDeEmergencia . modificarEscudos 100)

naveEspecial :: Nave
naveEspecial = UnaNave "Nave especial" 1500 1000 550 poderEspecial

poderEspecial :: Poder
poderEspecial = movimientoSuperTurbo . agregarEtiqueta " ultra premium"

agregarEtiqueta :: String -> Poder
agregarEtiqueta etiqueta nave = nave {nombre = nombre nave ++ etiqueta}

modificarDurabilidad :: Int -> Poder
modificarDurabilidad nro nave = nave {durabilidad = verificarSiEsNegativo nro (durabilidad nave)}

modificarAtaque :: Int -> Poder
modificarAtaque nro nave = nave {ataque = verificarSiEsNegativo nro (ataque nave)}

modificarEscudos :: Int -> Poder
modificarEscudos nro nave = nave {escudo = verificarSiEsNegativo nro (escudo nave)}

verificarSiEsNegativo :: Int -> Int -> Int
verificarSiEsNegativo nroOperado propiedadVariada
  | propiedadVariada + nroOperado < 0 = 0
  | otherwise = propiedadVariada + nroOperado

{- --------------------------- -}

{- Punto 2 -}
type Flota = [Nave]

durabilidadTotalFlota :: Flota -> Int
durabilidadTotalFlota flota = sum (map durabilidad flota)

{- Punto 3 -}

realizarAtaque :: Nave -> Nave -> Nave
realizarAtaque naveAtacante naveAtacada = dañoRecibido (poder naveAtacante naveAtacante) (poder naveAtacada naveAtacada)

dañoRecibido :: Nave -> Nave -> Nave
dañoRecibido naveAtacante naveAtacada
  | escudo naveAtacada > ataque naveAtacante = naveAtacada
  | otherwise = naveAtacada {durabilidad = durabilidad naveAtacada - calculoDeDaño naveAtacante naveAtacada}

calculoDeDaño :: Nave -> Nave -> Int
calculoDeDaño naveAtacante naveAtacada = ataque naveAtacante - escudo naveAtacada

{- Punto 4 -}

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave = durabilidad nave == 0

{- Punto 5 -}
type Estrategia = Nave -> Bool

realizarMisionSorpresa :: Nave -> Estrategia -> Flota -> Flota
realizarMisionSorpresa nave estrategia = map (realizarAtaque nave) . filter estrategia

navesDebiles :: Estrategia
navesDebiles nave = escudo nave < 200

navesConCiertaPeligrosidad :: Int -> Estrategia
navesConCiertaPeligrosidad valor nave = ataque nave > valor

naveConNombreCorto :: Estrategia -- estrategia inventada
naveConNombreCorto nave = length (nombre nave) < 5

{- Punto 6 -}
mayorMinimizacionDurabilidad :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
mayorMinimizacionDurabilidad nave flota estrategia1 estrategia2
  | durabilidadTotalFlota (filter estrategia1 flota) > durabilidadTotalFlota (filter estrategia2 flota) = realizarMisionSorpresa nave estrategia1 flota
  | otherwise = realizarMisionSorpresa nave estrategia2 flota

{- Punto 7 -}
{- Construir una flota infinita de naves.
¿Es posible determinar su durabilidad total?
No, la funcion durabilidadTotalFlota querrá sumar la durabilidad de todas las naves, las cuales son infinitas entonces nunca terminará de sumar. Es decir, al ser una flota infinita de naves no puede calcular la suma de sus durabilidades. El programa "explota".
¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente.
Sucede lo mismo que antes, va a querer filtrar las naves de la flota por una estrategia especifica y nunca va a poder parar de filtrar, por lo tanto ni siquiera llega a realizar el map y hacer la mision con la nave sobre las naves de la flota
-}

flotaInfinitaDeNaves :: Nave -> Flota
flotaInfinitaDeNaves nave = nave : flotaInfinitaDeNaves nave