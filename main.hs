import System.IO
import Archivos
import OpcionesGenerales
import OpcionesOperativas
import Menu
import Data.IORef
import Data.List (nubBy)
import Data.Function (on)

-- filtrarPorId :: String -> [[String]] -> [[String]]
-- filtrarPorId idBuscado lista = filter (\sublista -> sublista !! 2 == idBuscado) lista


-- Main
main :: IO ()
main = do
    let usuarios = []
    menuPrincipal usuarios
    {-
    facturarAux
    print "hola"
    -}
 