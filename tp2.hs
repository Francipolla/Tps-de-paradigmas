main :: IO ()
main = return ()

autorDeLibro :: (String,Int,String) -> String
autorDeLibro (autor,paginas,titulo) = autor
paginasDeLibro :: (String,Int,String) -> Int
paginasDeLibro (autor,paginas,titulo) = paginas
tituloDeLibro :: (String,Int,String) -> String 
tituloDeLibro (autor,paginas,titulo) = titulo

libro :: (String,Int,String) -> (String,Int,String)
libro (autor,paginas,titulo) = (autor,paginas,titulo)

masDe40 :: [Int] -> Bool
masDe40 paginas = any (40 < ) paginas

listaDeAutores :: [(String, Int, String)] -> [String]
listaDeAutores  = map autorDeLibro 

listaDeTitulos :: [(String, Int, String)] -> [String]
listaDeTitulos  = map tituloDeLibro 

listaDePaginas ::  [(String, Int, String)] -> [Int]
listaDePaginas  = map paginasDeLibro 

listaObligatoria :: [String]
listaObligatoria = ["Stephen King", "Christopher Paolini" , "Isaac Asimov"]
listaDeFantasia :: [String]
listaDeFantasia = ["Neil Gaiman", "Christopher Paolini"]

bibliotecaLigera :: [(String, Int, String)] -> Bool 
bibliotecaLigera biblioteca = not ((masDe40.listaDePaginas) biblioteca)

sumaDePaginas :: [(String, Int, String)] -> Float 
sumaDePaginas = (fromIntegral.sum.listaDePaginas)

cantidadDeLibros :: [(String, Int, String)] -> Float 
cantidadDeLibros = (fromIntegral.length.listaDeTitulos)

promedioDePaginas :: [(String, Int, String)] -> Float
promedioDePaginas biblioteca = (sumaDePaginas biblioteca) / (cantidadDeLibros biblioteca)

lecturaObligatoria :: (String,Int,String) -> Bool
lecturaObligatoria libro' = elem (autorDeLibro libro') listaObligatoria

bibliotecaFantasiosa :: [(String, Int, String)] -> Bool
bibliotecaFantasiosa biblioteca = listaDeAutores biblioteca == listaDeFantasia

vocales :: [Char]
vocales = ['a','e','i','o','u']

noEsVocal :: Char -> Bool
noEsVocal letra = not (elem letra vocales)

sinVocales :: String -> String
sinVocales libro' = filter noEsVocal libro'

bibliotecaSinVocales :: [(String, Int, String)] -> [String]
bibliotecaSinVocales biblioteca = map sinVocales (listaDeTitulos biblioteca)

nombreBiblioteca ::  [(String, Int, String)] -> String
nombreBiblioteca biblioteca = concat(bibliotecaSinVocales biblioteca)