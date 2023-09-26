module OpcionesGenerales where
import Data.IORef
import Archivos
import Data.List (nubBy, intercalate)
import Data.Function (on)


{-
recibe 2 listas de listas de strings y compara si las cedulas de la primera lista son iguales a las de la segunda lista
retorna una lista de listas de strings con los usuarios sin repetir si es que hay repetidos
ejemplo: [["55454464,"andy"],["5484448","juan"],["55454464,"andy"]] -> [["55454464,"andy"],["5484448","juan"]]
-}
-- Función que elimina duplicados basados en el primer valor de cada sublista
eliminarRepetidosPorPrimerValor :: [[String]] -> [[String]]
eliminarRepetidosPorPrimerValor = nubBy (\x y -> head x == head y)

-- Función que convierte la lista de listas en un solo string
convertirListaAString :: [[String]] -> String
convertirListaAString lista = intercalate "\n" (map (intercalate ",") lista)

-- cargar los usuarios desde un archivo indicado por el usuario
-- y los almacena en una lista de usuarios y lo retorna
cargarUsuarios :: IO [[String]]
cargarUsuarios = do
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    datos <- leerArchivo nombreArchivo
    lista2 <- cargarUsuariosDesdeArchivo "./data/usuarioss.csv"
    let lista = convertirStringALista datos
    let lista3 = lista ++ lista2
    let lista4 = eliminarRepetidosPorPrimerValor lista3
    let datos3 = convertirListaAString lista4
    escribirArchivo "./data/usuarios.txt" datos3
    return lista4

-- funcion de pruebas
cargarUsuariosAutomatico :: IO [[String]]
cargarUsuariosAutomatico = do
    datos <- leerArchivo "./data/prueba.csv"
    lista2 <- cargarUsuariosDesdeArchivo "./data/usuarios.txt"
    let lista = convertirStringALista datos
    let lista3 = lista ++ lista2
    let lista4 = eliminarRepetidosPorPrimerValor lista3
    let datos3 = convertirListaAString lista4
    print datos3
    escribirArchivo "./data/usuarios.txt" datos3
    return lista4



