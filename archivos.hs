module Archivos where
import Data.List.Split (splitOn)

-- función para obtener el contenido de un archivo
leerArchivo :: String -> IO String
leerArchivo nombre = do
    contenido <- readFile nombre
    return contenido

-- función para escribir un archivo
escribirArchivo :: String -> String -> IO ()
escribirArchivo nombre contenido = do
    writeFile nombre contenido

-- función para agregar al final de un archivo un string y un salto de linea
agregarArchivo :: String -> String -> IO ()
agregarArchivo nombre contenido = do
    appendFile nombre contenido
    appendFile nombre "\n"

{-
funcion que recibe un string y lo convierte en una lista de listas de strings
ejemplo: "hola,mundo\nhola,mundo" -> [["hola","mundo"],["hola","mundo"]]
ejemplo2: "hola mundo, adios mundo" -> [["hola mundo","adios mundo"]]
-}
-- Función que convierte un string en una lista de listas de strings
convertirStringALista :: String -> [[String]]
convertirStringALista input = map (splitOn ",") (lines input)


cargarUsuariosDesdeArchivo :: String -> IO [[String]]
cargarUsuariosDesdeArchivo nombre = do
    datos <- leerArchivo nombre
    let lista = convertirStringALista datos
    return lista