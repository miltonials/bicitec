module Archivos where
import Data.List.Split (splitOn)
import System.Directory (doesFileExist) -- para verificar si existe un archivo


{-
@function leerArchivo
@description: lee un archivo
@param nombre: nombre del archivo
@returns: IO String
-}
leerArchivo :: String -> IO String
leerArchivo nombre = do
    existe <- doesFileExist nombre
    if existe then do
        contenido <- readFile nombre
        return contenido
    else do
        putStrLn "El archivo no existe"
        return ""

{-
@function escribirArchivo
@description: escribe un archivo
@param nombre: nombre del archivo
@param contenido: contenido del archivo
@returns: IO ()
-}
escribirArchivo :: String -> String -> IO ()
escribirArchivo nombre contenido = do
    writeFile nombre contenido
    appendFile nombre "\n"

{-
@function agregarArchivo
@description: agrega contenido a un archivo
@param nombre: nombre del archivo
@param contenido: contenido del archivo
@returns: IO ()
-}
agregarArchivo :: String -> String -> IO ()
agregarArchivo nombre contenido = do
    appendFile nombre contenido
    appendFile nombre "\n"

{-
@function convertirStringALista
@description: convierte un string a una lista
@param input: string a convertir
@returns: [[String]]
-}  
convertirStringALista :: String -> [[String]]
convertirStringALista input = map (splitOn ",") (lines input)

{-
@function cargarUsuariosDesdeArchivo
@description: carga los usuarios desde un archivo
@param nombre: nombre del archivo
@returns: IO [[String]]
-}
cargarUsuariosDesdeArchivo :: String -> IO [[String]]
cargarUsuariosDesdeArchivo nombre = do
    datos <- leerArchivo nombre
    let lista = convertirStringALista datos
    return lista

{-
@function cargarArchivoEnLista
@description: carga un archivo en una lista
@param nombre: nombre del archivo
@returns: IO [[String]]
-}
cargarArchivoEnLista :: String -> IO [[String]]
cargarArchivoEnLista nombre = do
    datos <- leerArchivo nombre
    let lista = convertirStringALista datos
    return lista

{- 
@function cargarAlquileresSistema
@description: carga los alquileres del sistema
@returns: IO [[String]]
-}
cargarAlquileresSistema :: IO [[String]]
cargarAlquileresSistema = do
    alquileres <- leerArchivo "./data/alquileres.csv"
    let lista = convertirStringALista alquileres
    print lista
    return lista
    
{-
@function cargarFacturasSistema
@description: carga las facturas del sistema
@returns: IO [[String]]
-}
cargarFacturasSistema :: IO [[String]]
cargarFacturasSistema = do
    facturas <- leerArchivo "./data/facturas.csv"
    let lista = convertirStringALista facturas
    return lista
