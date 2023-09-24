module OpcionesOperativas where


--funcion de test
leerArchivoT1 :: String -> IO String
leerArchivoT1 nombre = do
    contenido <- readFile nombre
    return contenido