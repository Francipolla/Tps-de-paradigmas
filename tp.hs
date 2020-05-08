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

convertirFecha dia mes anio = Show dia mes anio
diaSencillo = even.(length.convertirFecha)

entregaSencilla dia mes anio = diaSencillo dia mes anio


inicialDeNombre nombreDeProducto = head nombreDeProducto 
largoDeNombreDeProducto nombreDeProducto = length nombreDeProducto
presenciaDeXEnNombre nombreDeProducto = elem 'x' nombreDeProducto
presenciaDeZEnNombre nombreDeProducto = elem 'z' nombreDeProducto
compararConVocales inicial = inicial == 'A' || inicial == 'E' || inicial == 'I' || inicial == 'O' || inicial == 'U'

sacarLetras nombreDeProducto = ((length nombreDeProducto)<10)-10
invertirNombre nombreDeProducto = reverse nombreDeProducto


productoDeLujo nombreDeProducto = presenciaDeXEnNombre nombreDeProducto || presenciaDeZEnNombre nombreDeProducto
productoCodiciado = largoDeNombreDeProducto > 10
productoCorriente =  compararConVocales.inicialDeNombre
productoXL nombreDeProducto = nombreDeProducto++"XL"


descodiciarProducto nombreDeProducto = sacarLetras nombreDeProducto

versionBarata = (reverse.descodiciarProducto)

productoDeElite nombreDeProducto = (productoDeLujo nombreDeProducto) && (productoCodiciado nombreDeProducto) && not(productoCorriente)

aplicarCostoDeEnvio costoDeEnvio precio = precio+costoDeEnvio
aplicarDescuento descuento precio = precio*descuento

precioTotal precioUnitario cantidad descuento costoDeEnvio = aplicarCostoDeEnvio(costoDeEnvio (aplicarDescuento descuento precioUnitario)*cantidad)