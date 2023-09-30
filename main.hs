import System.IO
import Archivos
import OpcionesGenerales
import OpcionesOperativas
import Menu
import Data.IORef
import Data.List (nubBy)
import Data.Function (on)


-- Main
main :: IO ()
main = do
    let usuarios = []
    menuPrincipal usuarios