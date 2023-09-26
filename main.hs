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
    {-
    -}
    let usuarios = []
    menuPrincipal usuarios
    -- let lista = [["B1","TR","transito"],["B2","AE","transito"],["B3","AG","3"],["B4","TR","4"],["B5","AE","5"],["B6","AG","6"],["B7","TR","7"],["B8","AE","8"],["B9","AG","9"],["B10","TR","2"],["B11","AE","3"],["B12","AG","5"],["B13","TR","1"],["B14","AE","2"],["B15","AG","7"],["B16","TR","7"]]
    -- let idBuscado = "3"
    -- let subListasConId = filtrarPorId idBuscado lista
      
    -- print subListasConId
 