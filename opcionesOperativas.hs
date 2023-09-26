module OpcionesOperativas where
import Archivos

-- SECCIÓN DE PARQUEOS 👇
cargarParqueos :: IO [[String]]
cargarParqueos = do
    -- let nombreArchivo = "./data/parqueos.csv"
    putStrLn "Ingrese el nombre del archivo: "
    nombreArchivo <- getLine
    datos <- leerArchivo nombreArchivo
    let lista = convertirStringALista datos
    return lista

-- [["1","Parqueo el ciclista","Barrio Escalante","SJ","9.9378","-84.0646"],["2","Bicitec central","Limon","LI","10.0000","-83.0333"],["3","Cartaguito campeon","Cartago","CA","9.8572","-83.9290"],["4","El caribenho","Paraiso de sixaola","LI","9.5204","-82.9735"],["5","El ciclista feliz","Barrio Escalante","SJ","9.9378","-84.0646"],["6","El grito guanacasteco","Guanacaste","GU","10.6348","-85.4409"],["7","Vacaniones felices","Puntarenas","PU","9.9768","-84.8386"],["8","El melcochon","Alajuela","AL","10.0169","-84.2142"],["9","El tajador","Heredia","HE","10.0029","-84.1160"]]
-- [id, nombre, provincia, latitud, longitud]
mostrarParqueos :: [[String]] -> IO ()
mostrarParqueos parqueos = do
    putStrLn "Lista de parqueos: "
    mostrarParqueosAux parqueos 1

-- función auxiliar para mostrar los parqueos
mostrarParqueosAux :: [[String]] -> Int -> IO ()
mostrarParqueosAux [] _ = do
    putStrLn "Fin de la lista de parqueos"

mostrarParqueosAux (x:xs) contador = do
    putStrLn ("Parqueo #" ++ show contador)
    putStrLn ("ID: " ++ head x)
    putStrLn ("Nombre: " ++ x !! 1)
    putStrLn ("Dirección: " ++ x !! 2)
    putStrLn ("Provincia: " ++ x !! 3)
    -- putStrLn ("Latitud: " ++ x !! 4)
    -- putStrLn ("Longitud: " ++ x !! 5)
    putStrLn("Coordenadas: " ++ x !! 4 ++ ", " ++ x !! 5)
    putStrLn ""
    mostrarParqueosAux xs (contador + 1)
    
