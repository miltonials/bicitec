import System.IO
import Menu
import Archivos
import OpcionesGenerales
import Data.IORef
import Data.List (nubBy)
import Data.Function (on)



-- Main
main :: IO ()
main = do
    let usuarios = []
    menuPrincipal usuarios
    --escribirArchivo :: String -> String -> IO ()
    -- escribirArchivo "./data/prueba.txt" "Hola mundo\n"

    --agregarArchivo :: String -> String -> IO ()
    -- agregarArchivo "./data/prueba.txt" "Adios mundo\n"
