import Text.Show.Functions ()

-- Punto 1: Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo
type Gema = Personaje -> Personaje -- Le hace algo al personaje

data Personaje = UnPersonaje
  { edad :: Int,
    energia :: Int,
    serieHabilidades :: [String],
    nombre :: String,
    planeta :: String
  }
  deriving (Show, Eq)

data Guantelete = UnGuantelete
  { material :: String,
    gemas :: [Gema]
  }
  deriving (Show)

-- guanteleteCompleto :: Guantelete
-- guanteleteCompleto = UnGuantelete "uru" [mente, alma, espacio, poder, tiempo, gemaLoca]

type Universo = [Personaje]

ironMan :: Personaje
ironMan = UnPersonaje 20 500 ["usar la espada", "controlar la mente", "romper", "inmortalidad"] "Iron Man" "Tierra"

drStrange :: Personaje
drStrange = UnPersonaje 25 50 ["usar la espada"] "Dr Strange" "Venus"

groot :: Personaje
groot = UnPersonaje 30 10 ["decir soy groot"] "Groot" "Jupiter"

wolverine :: Personaje
wolverine = UnPersonaje 35 5 ["araniar"] "Wolverine" "Saturno"

viudaNegra :: Personaje
viudaNegra = UnPersonaje 55 15 ["usar la espada", "enviudar"] "Viuda Negra" "Marte"

universo1 :: Universo
universo1 = [ironMan, drStrange, groot, wolverine, viudaNegra]

intentarChasquearUniverso :: Guantelete -> Universo -> Universo
intentarChasquearUniverso guantelete universo
  | esChasqueable guantelete = chasquearUniverso universo
  | otherwise = error "No se puede chasquear el universo"

esChasqueable :: Guantelete -> Bool
esChasqueable guantelete = length (gemas guantelete) == 6 && material guantelete == "uru"

chasquearUniverso :: Universo -> Universo
chasquearUniverso = mitadDeLaLista

mitadDeLaLista :: [a] -> [a]
mitadDeLaLista lista = take (length lista `div` 2) lista

mostrarEdadesPersonajesUniverso :: Universo -> [Int]
mostrarEdadesPersonajesUniverso = map edad

-- Punto 2: Resolver utilizando únicamente orden superior
-- a) Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any ((< 45) . edad)

-- b) Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.
energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso = sumatoriaEnergias . integrantesMasDeUnaHabilidad

integrantesMasDeUnaHabilidad :: Universo -> Universo
integrantesMasDeUnaHabilidad = filter (\x -> length (serieHabilidades x) > 1)

sumatoriaEnergias :: Universo -> Int
sumatoriaEnergias universo = sum $ map energia universo

{- Parte 2 -}
-- Modelar gemas
-- La mente, tiene la habilidad de debilitar la energía de un usuario en un valor dado
mente :: Int -> Gema
mente = quitarEnergia

-- El alma, puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía.
alma :: String -> Gema
alma habilidad = quitarEnergia 10 . eliminarHabilidad habilidad

eliminarHabilidad :: String -> Gema
eliminarHabilidad habilidad personaje = personaje {serieHabilidades = filter (/= habilidad) (serieHabilidades personaje)}

quitarEnergia :: Int -> Gema
quitarEnergia nro personaje = personaje {energia = energia personaje - nro}

-- El espacio, que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía
espacio :: String -> Gema
espacio nuevoPlaneta = quitarEnergia 20 . transportarDePlaneta nuevoPlaneta

transportarDePlaneta :: String -> Gema
transportarDePlaneta nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

-- El poder, deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
poder :: Gema
poder personaje
  | length (serieHabilidades personaje) <= 2 = personaje {energia = 0, serieHabilidades = []}
  | otherwise = personaje {energia = 0}

-- El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, no puede dejar la edad del oponente con menos de 18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
tiempo :: Gema
tiempo personaje
  | edad personaje `div` 2 < 18 = quitarEnergia 50 personaje {edad = 18}
  | otherwise = quitarEnergia 50 personaje {edad = edad personaje `div` 2}

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.
gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema

{- Parte 3 -}
-- Implementar las gemas del infinito, evitando lógica duplicada
listaGemas :: [Gema]
listaGemas = [gemaLoca (alma "inmortalidad"), mente 10, alma "romper", espacio "neptuno", poder, tiempo]

gemasDelInfinito :: [Gema] -> Gema
gemasDelInfinito listaDeGemas personaje = foldr ($) personaje listaDeGemas

{- Parte 4: -}
-- Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”
personajeNuevo :: Personaje
personajeNuevo = UnPersonaje 25 50000 ["usar la espada", "usar Mjolnir", "programacion en Haskell"] "Nuevo Personaje" "Urano"

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = UnGuantelete "pepe" [tiempo, alma "usar Mjolnir", gemaLoca (poder . alma "programacion en Haskell")]

{- Parte 5: (No se puede utilizar recursividad) -}
-- Generar la función utilizar que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.
-- ¿¿¿Seria lo  de la parte 3?????

{- Parte 6: (Utilizar recursividad) -}
-- dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima
gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelete = filtrarGemaMasPoderosa personaje (gemas guantelete)

filtrarGemaMasPoderosa :: Personaje -> [Gema] -> Gema
filtrarGemaMasPoderosa _ [g] = g
filtrarGemaMasPoderosa personaje (g1 : g2 : gs)
  | energia (g1 personaje) < energia (g2 personaje) = filtrarGemaMasPoderosa personaje (g1 : gs)
  | otherwise = filtrarGemaMasPoderosa personaje (g2 : gs)

{-  Punto 7: Dada la función generadora de gemas y un guantelete de locos: -}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema : infinitasGemas gema

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas tiempo)

-- Y la función:
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas = gemasDelInfinito . take 3 . gemas

-- Justifique si se puede ejecutar, relacionándolo con conceptos vistos en la cursada:
-- ● gemaMasPoderosa ironMan guanteleteDeLocos
-- No se puede ejecutar, me muestra por pantalla una función que seria la funcion Gema (Personaje a Personaje). Nunca se termina de decidir cual es la gema mas poderosa al ser una lista infinita de gemas donde se van a evaluar infinitas gemas en la funcion recursiva
-- ● usoLasTresPrimerasGemas guanteleteDeLocos ironMan
-- Si se puede ejecutar, debido al "lazy loading" de Haskell al tener que evaluar solamente las primeras 3 gemas de una lista infinita de gemas, me permite devolver el efecto de las primeras 3 gemas de la lista infinita de gemas por sobre el personaje indicado