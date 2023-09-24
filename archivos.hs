module Archivos where

-- función para obtener el contenido de un archivo
leerArchivo :: String -> IO String
leerArchivo nombre = do
    contenido <- readFile nombre
    return contenido

-- función para escribir un archivo
escribirArchivo :: String -> String -> IO ()
escribirArchivo nombre contenido = do
    writeFile nombre contenido

-- función para agregar al final de un archivo
agregarArchivo :: String -> String -> IO ()
agregarArchivo nombre contenido = do
    appendFile nombre contenido
