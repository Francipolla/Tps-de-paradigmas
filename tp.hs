import Text.Show.Functions

main :: IO ()
main = return ()

{--
take ::
>> take 2 “Buenas!!”
“Bu”
>> take 5 “Saludos”
“Salud”

drop ::
>> drop 2 “Buenas!!”
“enas!!”
>> drop 3 “Saludos”
“udos”
	
head ::
>> head “Buenas!!”
‘B’
>> head “Nos vemos”
‘N’

elem ::
	>> elem ‘a’ “Buenas!!”
	True
	>> elem ‘y’ “Buenas!!”
	False

reverse ::
	>> reverse “Buenas!!”
	"!!saneuB"
--}

convertirDia    ::  Show a => a -> String
convertirDia  dia = show dia ++ "/"

convertirMes    ::  Show a => a -> String
convertirMes  mes = show mes ++ "/"

convertirAnio   ::  Show a => a -> String
convertirAnio  anio = show anio

convertirFecha  :: Int -> Int -> Int -> String
convertirFecha dia mes anio = (convertirDia dia) ++ (convertirMes mes) ++ (convertirAnio anio)

diaSencillo     :: Int -> Int -> Int -> Bool 
diaSencillo dia mes anio = even (length (convertirFecha dia mes anio))

entregaSencilla :: Int -> Int -> Int -> Bool 
entregaSencilla dia mes anio = diaSencillo dia mes anio

inicialDeNombre :: String -> Char
inicialDeNombre nombreDeProducto = head nombreDeProducto

largoDeNombreDeProducto :: String -> Int
largoDeNombreDeProducto nombreDeProducto = length nombreDeProducto

presenciaDeXEnNombre    :: String -> Bool
presenciaDeXEnNombre nombreDeProducto = elem 'x' nombreDeProducto

presenciaDeZEnNombre    :: String -> Bool
presenciaDeZEnNombre nombreDeProducto = elem 'z' nombreDeProducto

compararConVocales      :: Char -> Bool
compararConVocales inicial = inicial == 'A' || inicial == 'E' || inicial == 'I' || inicial == 'O' || inicial == 'U'

sacarLetras :: String -> String
sacarLetras nombre | (length nombre) > 10 = take 10 nombre
                   | otherwise =  nombre
                   
invertirNombre    :: String -> String
invertirNombre nombreDeProducto = reverse nombreDeProducto

productoCodiciado :: String -> Bool
productoCodiciado nombreDeProducto = (largoDeNombreDeProducto nombreDeProducto) > 10

productoCorriente :: String -> Bool
productoCorriente nombreDeProducto =  (compararConVocales.inicialDeNombre)nombreDeProducto

productoXL :: String -> String
productoXL nombreDeProducto = nombreDeProducto++"XL"

descodiciarProducto :: String -> String
descodiciarProducto nombreDeProducto = sacarLetras nombreDeProducto

versionBarata :: String -> String
versionBarata nombreDeProducto = (reverse.descodiciarProducto) nombreDeProducto

productoDeElite :: String -> Bool
productoDeElite nombreDeProducto = (productoDeLujo nombreDeProducto) && (productoCodiciado nombreDeProducto) && (not(productoCorriente nombreDeProducto))

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio costoDeEnvio precio = precio+costoDeEnvio

aplicarDescuento :: Num a => a -> a -> a
aplicarDescuento descuento precio = precio*descuento

precioTotal ::  Num a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento costoDeEnvio = ((aplicarCostoDeEnvio costoDeEnvio).(aplicarDescuento descuento)) (precioUnitario*cantidad)

productoDeLujo     :: String -> Bool
productoDeLujo nombreDeProducto = (presenciaDeXEnNombre nombreDeProducto) || (presenciaDeZEnNombre nombreDeProducto)
