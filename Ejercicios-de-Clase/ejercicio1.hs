-- Ejercicio 1

data Persona = UnaPersona
  { nombre :: String,
    edad :: Int,
    presente :: Bool,
    cursando :: [Materia]
  }
  deriving (Show, Eq)

data Materia = UnaMateria
  { descripcion :: String,
    indice :: Float
  }
  deriving (Show, Eq)

marcos :: Persona
marcos = UnaPersona "Marcos" 20 True [UnaMateria "Fisica" 8.5, UnaMateria "Algoritmos" 9.5, UnaMateria "Quimica" 7.5, UnaMateria "Ingenieria y Sociedad" 10]

lucas :: Persona
lucas = UnaPersona "Lucas" 24 False [UnaMateria "Fisica" 6, UnaMateria "Algoritmos" 8, UnaMateria "Quimica" 5]

-- Agregar ejemplos de materias y nuevas personas que cursen algunas de ellas
-- Nuevas personas
luciano :: Persona
luciano = UnaPersona "Luciano" 19 True [UnaMateria "Geografia" 5.5, UnaMateria "Sociales" 8.2, UnaMateria "Sintaxis" 5]

german :: Persona
german = UnaPersona "German" 29 False [UnaMateria "Discreta" 6.5, UnaMateria "SyO" 8, UnaMateria "Arquitectura de Computadores" 7.5]

-- Nuevas materias
matematica :: Materia
matematica = UnaMateria "Matematica" 7

geografia :: Materia
geografia = UnaMateria "Geografia" 6

ingles :: Materia
ingles = UnaMateria "Ingles" 5

-- Queremos averiguar:

-- quienCursaMas
-- dadas dos personas, cual de ellas cursa mayor cantidad de materias (problema si los dos cursan la misma cantidad)
quienCursaMas :: Persona -> Persona -> Persona
{- quienCursaMas (UnaPersona n1 e1 p1 c1) (UnaPersona n2 e2 p2 c2)
  | length c2 > length c1 = UnaPersona n2 e2 p2 c2
  | otherwise = UnaPersona n1 e1 p1 c1 -}
quienCursaMas persona1 persona2
  | length (cursando persona1) > length (cursando persona2) = persona1
  | otherwise = persona2

-- felizCumple
-- hace que una persona cumpla años
cumplirAnios :: Persona -> Persona
-- cumplirAnios (UnaPersona n e p c) = UnaPersona n (e + 1) p c
cumplirAnios persona = persona {edad = edad persona + 1}

-- inscribirse a materia
-- dada una persona y una materia, obtener cómo queda la persona cursando esa materia
inscribirseAUnaMateria :: Persona -> Materia -> Persona
-- inscribirseAUnaMateria (UnaPersona n e p c) (UnaMateria d i) = UnaPersona n e p (c ++ [UnaMateria d i])
inscribirseAUnaMateria persona materia = persona {cursando = cursando persona ++ [materia]}

-- vaMejorandoLaCarrera
-- es cierto para un persona dada, si su ultima materia cursada tiene mejor índice que la primera que curso
vaMejorandoLaCarrera :: Persona -> Bool
-- vaMejorandoLaCarrera (UnaPersona n e p c) = indice (last c) > indice (head c)
vaMejorandoLaCarrera persona = indice (last (cursando persona)) > indice (head (cursando persona))
