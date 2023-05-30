-- import Data.List
import Text.Read.Lex (Number)
import Text.Show.Functions ()

data Serie = UnaSerie
  { nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
  }
  deriving (Eq, Show)

tioGolpetazo :: Serie
tioGolpetazo =
  UnaSerie
    { nombre = "One punch man",
      genero = "Monito chino",
      duracion = 24,
      cantTemporadas = 1,
      calificaciones = [5],
      esOriginalDeNetflis = False
    }

cosasExtranias :: Serie
cosasExtranias =
  UnaSerie
    { nombre = "Stranger things",
      genero = "Misterio",
      duracion = 50,
      cantTemporadas = 2,
      calificaciones = [3, 3],
      esOriginalDeNetflis = True
    }

dbs :: Serie
dbs =
  UnaSerie
    { nombre = "Dragon ball supah",
      genero = "Monito chino",
      duracion = 150,
      cantTemporadas = 5,
      calificaciones = [5, 3, 2, 4],
      esOriginalDeNetflis = False
    }

espejoNegro :: Serie
espejoNegro =
  UnaSerie
    { nombre = "Black mirror",
      genero = "Suspenso",
      duracion = 123,
      cantTemporadas = 1,
      calificaciones = [2, 2, 2],
      esOriginalDeNetflis = True
    }

rompiendoMalo :: Serie
rompiendoMalo =
  UnaSerie
    { nombre = "Breaking Bad",
      genero = "Drama",
      duracion = 200,
      cantTemporadas = 5,
      calificaciones = [1],
      esOriginalDeNetflis = False
    }

treceRazonesPorque :: Serie
treceRazonesPorque =
  UnaSerie
    { nombre = "13 reasons why",
      genero = "Drama",
      duracion = 50,
      cantTemporadas = 1,
      calificaciones = [3, 3, 5, 1],
      esOriginalDeNetflis = True
    }

-- Parte 1: Listas básicas

{- 1- Crear una maratón con los ejemplos dados. Una maratón es una colección de series. -}
type Maraton = [Serie]

maraton1 :: Maraton
maraton1 = [tioGolpetazo, dbs, espejoNegro]

maraton2 :: Maraton
maraton2 = [cosasExtranias, dbs, rompiendoMalo, treceRazonesPorque]

maraton3 :: Maraton
maraton3 = [tioGolpetazo, espejoNegro]

{- 2- Saber la cantidad de series del maratón -}
cantSeriesMaraton :: Maraton -> Int
cantSeriesMaraton = length

{- 3- Saber si una serie es popular: una serie se considera popular si recibió 3 o más calificaciones. -}
esPopular :: Serie -> Bool
esPopular serie = length (calificaciones serie) >= 3

{- 4- Averiguar si una serie vale la pena, es decir, si tiene más de una temporada y tiene 3 o más calificaciones. -}
valeLaPenaSerie :: Serie -> Bool
valeLaPenaSerie serie = (cantTemporadas serie > 1) && esPopular serie

{- 5- Saber si una maratón vale la pena: una maratón vale la pena si la primera y la última serie de la maratón valen la pena, o bien si está el drama "Breaking Bad", con 5 temporadas y 200 minutos, sin calificar, que no es original de netflis. -}
valeLaPenaMaraton :: Maraton -> Bool
valeLaPenaMaraton maraton = valeLaPenaSerie (head maraton) && valeLaPenaSerie (last maraton) || elem rompiendoMalo maraton

{- 6- Averiguar si una maratón repunta al final, lo que sucede si la primera mitad de las series constituyen una maratón que no valdría la pena, pero la segunda mitad sí. (si es cantidad impar de series de la maratón, la segunda mitad tiene una serie más que la primera) -}
maratonRepuntaAlFinal :: Maraton -> Bool
maratonRepuntaAlFinal maraton = not (valeLaPenaMaraton . primerMitadDeLasSeries $ maraton) && (valeLaPenaMaraton . segundaMitadDeLasSeries $ maraton)

primerMitadDeLasSeries :: Maraton -> Maraton
primerMitadDeLasSeries maraton = take (length maraton `div` 2) maraton

segundaMitadDeLasSeries :: Maraton -> Maraton
segundaMitadDeLasSeries maraton = drop (length maraton `div` 2) maraton

{- 7- Calcular la calificación de una serie. Es el promedio de las calificaciones recibidas, (redondeado hacia abajo) -}
promCalificaciones :: Serie -> Int
promCalificaciones serie = sum (calificaciones serie) `div` length (calificaciones serie)

{- 8- Obtener la dispersión de las calificaciones de la serie, que es la diferencia entre la mejor y peor calificación. (Si todas las calificaciones son coincidentes, se deduce que la dispersión es 0), -}
-- dispersionCalificaciones :: Serie -> Int
-- dispersionCalificaciones serie = mejorCalificacion (calificaciones serie) - peorCalificacion (calificaciones serie)
dispersionCalificaciones :: Serie -> Int
dispersionCalificaciones serie = maximum (calificaciones serie) - minimum (calificaciones serie)

-- mejorCalificacion :: [Int] -> Int
-- mejorCalificacion = foldl max 0

{- mejorCalificacion [c] = c
mejorCalificacion (c1 : c2 : cs)
  | c1 > c2 = mejorCalificacion (c1 : cs)
  | otherwise = mejorCalificacion (c2 : cs) -}

-- peorCalificacion :: [Int] -> Int
-- peorCalificacion = foldl min 5

-- peorCalificacion [c] = c
-- peorCalificacion (c1 : c2 : cs)
--   | c1 < c2 = peorCalificacion (c1 : cs)
--   | otherwise = peorCalificacion (c2 : cs)

{- 9- Calificar una serie, que significa agregar una nueva calificación al final de las anteriores. -}
calificarSerie :: Int -> Serie -> Serie
calificarSerie calificacion serie
  | calificacion <= 5 = serie {calificaciones = calificaciones serie ++ [calificacion]}
  | otherwise = serie

{- 10- Hypear una serie: cuando se hypea una serie, se alteran la primer y última calificación recibida, aumentándola en 2 estrellas (recordá que la escala de calificación es de 5 estrellas máximo). Si la serie recibió alguna calificación de 1 estrella, no se puede hypear -}
hypearSerie :: Serie -> Serie
hypearSerie serie
  | elem 1 (calificaciones serie) = serie
  | otherwise = serie {calificaciones = calificacionModificada (calificaciones serie)}

calificacionModificada :: [Int] -> [Int]
calificacionModificada [c] = [c + 2]
calificacionModificada (c1 : cs) = c1 + 2 : init cs ++ [last cs + 2]

-- Parte 2: Jugando más con listas

{- 1- Obtener todas las series que sean de monitos chinos -}
seriesDeMonitosChinos :: Maraton -> Maraton
seriesDeMonitosChinos = filter (\x -> genero x == "Monito chino")

{- 2- Obtener las series originales de Netflis que valen la pena. -}
seriesNValenLaPena :: Maraton -> Maraton
seriesNValenLaPena = listaSeriesValenLaPena . seriesOriginalesDeNetflis

listaSeriesValenLaPena :: Maraton -> Maraton
listaSeriesValenLaPena = filter valeLaPenaSerie

seriesOriginalesDeNetflis :: Maraton -> Maraton
seriesOriginalesDeNetflis = filter esOriginalDeNetflis

{- 3- Obtener las series que tengan una cantidad n de temporadas -}
seriesNTemporadas :: Int -> Maraton -> Maraton
seriesNTemporadas n = filter (\x -> cantTemporadas x == n)

{- 4- Saber si una maratón (una lista de series) es flojita: una maratón se considera flojita cuando todas sus series son de 1 temporada -}
esFlojita :: Maraton -> Bool
esFlojita maraton = length (seriesNTemporadas 1 maraton) == length maraton

{- 5- Dada una maratón, saber cuánto tiempo se tarda en ver esa maratón completa -}
tiempoDeMaraton :: Maraton -> Int
tiempoDeMaraton = sum . map duracion

{- 6- Actualizar la forma de saber si una maratón vale la pena: una maratón vale la pena si al menos una serie de la maratón vale la pena; o bien, si Breaking Bad forma parte de la maratón. -}
valeLaPenaMaratonActualizada :: Maraton -> Bool
valeLaPenaMaratonActualizada maraton = any valeLaPenaSerie maraton || elem rompiendoMalo maraton

{- 7- Dada una maratón de series, saber la calificación más alta que se le dio a una serie original de Netflis -}
mayorCalificacionMaraton :: Maraton -> Int
mayorCalificacionMaraton = calificacionMasAlta . seriesOriginalesDeNetflis

calificacionMasAlta :: Maraton -> Int
calificacionMasAlta = maximum . map (maximum . calificaciones)

{- 8- Dada una maratón de series, hypear las series que corresponda. Una serie debe ser hypeada si es de drama o de suspenso -}
hypearMaraton :: Maraton -> Maraton
hypearMaraton = map hypearSerie . condicionSerieHypeada

condicionSerieHypeada :: Maraton -> Maraton
condicionSerieHypeada = filter (\x -> genero x == "Drama" || genero x == "Suspenso")

--  Parte 3: Alto orden
-- 1- Se quieren obtener más promedios, en todos los casos, redondeados hacia abajo:
{- a- Obtener el promedio de duración de las series de una maratón (considerar duración por cantidad de temporadas). -}
promedioDuracionSeries :: Maraton -> Int
promedioDuracionSeries maraton = (tiempoDeMaraton maraton * duracionPorCantidadDeTemporadas maraton) `div` length maraton -- Supongo que es algo así

duracionPorCantidadDeTemporadas :: Maraton -> Int
duracionPorCantidadDeTemporadas = sum . map cantTemporadas

{- b- Obtener la calificación de una maratón, que es el promedio de calificaciones de las series que componen la maratón. -}

{- c- Obtener el promedio de calificaciones de una lista de maratones. -}

-- 2- También se buscan algunos records:
{- a- La serie mejor calificada de una maratón -}
-- serieMejorCalificada :: Maraton -> Serie
-- serieMejorCalificada :: Maraton -> Serie
-- serieMejorCalificada maraton = serie {calificaciones = [calificacionMasAlta maraton]}

{- b- La serie más larga de una maratón -}

{- c- Dada una lista de maratones, encontrar la mejor de todas, que es la maratón que tiene la mejor calificación. -}

-- 3- Aparecen los críticos de series, que se especializan en analizar y establecer calificaciones. Cada crítico tiene su preferencia respecto de cuáles series calificar y cómo hacerlo. Por ejemplo, están los siguientes:
{- D. Moleitor: Se especializa en series flojitas, elimina todas sus calificaciones mayores a 3, si hubiera alguna, y agrega una calificacion 1 al final. -}

{- Hypeador: A todas las series que se pueden hypear, las hypea (como se explicó anteriormente). Exquisito: Prefiere las series que valen la pena. Sustittuye todas sus calificaciones recibidas por un nueva lista con una única calificación, que es el promedio de calificaciones más uno. -}

{- CualquierColectivoLoDejaBien: A todas las series les agrega una calificación de 5. -}