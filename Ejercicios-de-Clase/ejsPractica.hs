{- 1) Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomó la frecuencia cardíaca de uno
de los participantes obteniéndose un total de 7 muestras que son las siguientes:

frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
Comienza con un frecuencia de 80 min 0.
A los 10 min la frecuencia alcanza los 100,
A los 20 min la frecuencia es de 120,
A los 30 min la frecuencia es de 128,
A los 40 min la frecuencia es de 130, …etc..
A los 60 min la frecuencia es de 125 frecuenciaCardiaca es un función constante.
  -}

frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

-- a) Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca.
-- Main> promedioFrecuenciaCardiaca
-- 115.285714285714
promedioFrecuenciaCardiaca :: [Int] -> Float
promedioFrecuenciaCardiaca frecuencias = fromIntegral (sum frecuencias) / fromIntegral (length frecuencias)

-- b) Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60.
-- Main> frecuenciaCardiacaMomento 30
-- 128
-- Ayuda: Vale definir una función auxiliar para conocer el número de muestra.
frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! muestraEnMinuto m

-- Obtiene el número de muestra correspondiente a un minuto dado
muestraEnMinuto :: Int -> Int
muestraEnMinuto m = div m 10

-- c) Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m.
-- Main> frecuenciasHastaMomento 30
-- [80, 100, 120, 128]
-- Ayuda: Utilizar la función take y la función auxiliar definida en el punto anterior.
frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento m = take (muestraEnMinuto m + 1) frecuenciaCardiaca
