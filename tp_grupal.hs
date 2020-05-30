import Text.Show.Functions

main :: IO ()
main = return ()

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base

dia :: Fecha -> Int
dia (day,_,_) = day

mes :: Fecha -> Int
mes (_,month,_) = month

anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente         :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm             :: Float,
 temperaturaAgua :: Int,
 ultimoArreglo   :: Fecha
} deriving (Show, Eq)


-- Punto 1 (Costo de reparacion de un auto)

listaDeLetras :: String -> [String]
listaDeLetras [] = []
listaDeLetras abc = ((:).(take 1) $ abc) (listaDeLetras (drop 1 abc))

abecedario :: String
abecedario = ['A'..'Z']

inicialesDeCalculoPatental :: String
inicialesDeCalculoPatental = ['D'..'N']

armarPatente :: String -> String -> [String]
armarPatente [] abc = []
armarPatente iniciales abc = ((map ((take 1 iniciales)++)).listaDeLetras $ abc) ++ (armarPatente (drop 1 iniciales) abc)

calculoPatental :: [String]
calculoPatental = ((++).(armarPatente "D") $ ['J'..'Z']) $ ((++).(armarPatente ((drop 1).(take ((subtract 2).length $ inicialesDeCalculoPatental)) $ inicialesDeCalculoPatental)) $ abecedario)  (armarPatente "N" "AB")

costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto | (((any (==((take 2).letrasDePatente $ unAuto))) $ calculoPatental)) && ((( == "4").(drop 3).patente $ unAuto) ||  (( == "4").(drop 5).patente $ unAuto)) = (3000*).largoDePatente $ unAuto 
                         | (((any (==((take 2).letrasDePatente $ unAuto))) $ calculoPatental)) && ((( /= "4").(drop 3).patente $ unAuto) ||  (( /= "4").(drop 5).patente $ unAuto)) = 20000
                         | (==7).largoDePatente $ unAuto                                                                                                           = 12500
                         | otherwise                                                                                                                               = 15000

largoDePatente :: Auto -> Int
largoDePatente unAuto = length.patente $ unAuto

letrasDePatente :: Auto -> String
letrasDePatente unAuto | (/=7).largoDePatente $ unAuto = (++) ((take 2).patente $ unAuto) ((drop 5).patente $ unAuto)
                       | otherwise                     = (take 3).patente $ unAuto

-- Casos de prueba --


{--
auto1 :: Auto
auto1 = Auto "AT001LN" [0.5,0.3,0.8,0] 0 0 (0,0,0)
auto2 :: Auto
auto2 = Auto "DJV214" [] 0 0 (0,0,0)
auto3 :: Auto
auto3 = Auto "DJV215" [] 0 0 (0,0,0)
auto4 :: Auto
auto4 = Auto "DFH029" [] 0 0 (0,0,0)
--}

-- Punto 2 --

-- Parte 1 --
primeraLlanta :: Auto -> Float 
primeraLlanta = head.desgasteLlantas 

autoPeligroso :: Auto -> Bool
autoPeligroso  = (>0.5).primeraLlanta  

-- Parte 2 --
necesitaRevision :: Auto -> Bool 
necesitaRevision = (<= 2015).anio.ultimoArreglo 

-- Punto 3 --

-- Parte 1 --
type Mecanico = Auto -> Auto

alfa :: Mecanico
alfa unAuto | (>2000).rpm $ unAuto = unAuto{rpm = 2000}
            | otherwise            = unAuto

bravo :: Mecanico
bravo unAuto = unAuto{desgasteLlantas = [0,0,0,0]}

charly :: Mecanico
charly = bravo.alfa

-- Parte 2 --

segundaLlanta :: Auto -> [Float] 
segundaLlanta = (drop 1).(take 2).desgasteLlantas  

terceraLlanta :: Auto -> [Float]
terceraLlanta = (drop 2).(take 3).desgasteLlantas

cuartaLlanta :: Auto -> [Float]
cuartaLlanta  = (drop 3).desgasteLlantas

tango :: Mecanico
tango unAuto = unAuto

zulu :: Mecanico
zulu unAuto = lima unAuto{temperaturaAgua = 90}

lima :: Mecanico
lima unAuto = unAuto{desgasteLlantas = (0 :).(0 :).((terceraLlanta unAuto)++).cuartaLlanta $ unAuto}

-- Punto 4 --

autosImpares :: [Auto] -> [Auto] 
autosImpares [] = []
autosImpares unaLista = (head unaLista) : autosImpares (drop 2 unaLista)

autosPares :: [Auto] -> [Auto]
autosPares unaLista = filter (\auto -> (notElem auto).autosImpares $ unaLista) unaLista

ordenarTOC :: [Auto] -> Bool
ordenarTOC unAuto = ((all odd).(map (round.(10*).sum)).(map desgasteLlantas).autosImpares $ unAuto) && ((all even).(map (round.(10*).sum)).(map desgasteLlantas).autosPares $ unAuto)

--Casos de prueba--
{--
autoa :: Auto
autoa = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)
autob :: Auto
autob = Auto "AT001LN"  [0.2, 0.5, 0.6, 0.1] 0 0 (0,0,0) 
autoc :: Auto
autoc = Auto "AT001LN"  [0.1, 0.1, 0.1, 0] 0 0 (0,0,0)
autod :: Auto
autod = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)


autof :: Auto
autof = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)
autog :: Auto
autog = Auto "AT001LN"  [0.3, 0.5, 0.6, 0.1] 0 0 (0,0,0)
autoh :: Auto
autoh = Auto "AT001LN"  [0.1, 0.1, 0.1, 0] 0 0 (0,0,0)

autoi :: Auto
autoi = Auto "AT001LN"  [0.1, 0.4, 0.2, 0] 0 0 (0,0,0)
autoj :: Auto
autoj = Auto "AT001LN"  [0.1, 0.4, 0.2, 0.1] 0 0 (0,0,0)

lista1 :: [Auto]
lista1 = [autoa,autob,autoc,autod]

lista2 :: [Auto]
lista2 = [autof,autog,autoh]

lista3 :: [Auto]
lista3 = [autoi] 

lista4 :: [Auto]
lista4 = [autoj] --}

-- Punto 5 --

ordenDeReparacion :: Auto -> Fecha -> [Mecanico] -> Auto
ordenDeReparacion unAuto unaFecha listaDeMecanicos = (foldr1 (.) listaDeMecanicos $ unAuto{ultimoArreglo = unaFecha}) 

-- Punto 6 --

-- Parte 1 --

dejanEnCondiciones :: Auto -> [Mecanico] -> [Mecanico]
dejanEnCondiciones unAuto listaMecanicos = filter (\mecanico ->  (0 ==).primeraLlanta.mecanico $ unAuto) listaMecanicos 

-- Parte 2 --

costoDeGarage :: [Auto] -> Int
costoDeGarage listaAutos |any necesitaRevision listaAutos = sum.(map costoDeReparacion).(filter necesitaRevision) $ listaAutos
                         |otherwise                    = 0

-- Prueba --

{--
auto1 :: Auto
auto1 = Auto "AT001LN" [0.5,0.3,0.8,0] 0 0 (0,0,0)
auto2 :: Auto
auto2 = Auto "DJV214" [] 0 0 (0,0,2016)
auto3 :: Auto
auto3 = Auto "DJV215" [] 0 0 (0,0,2016)
auto4 :: Auto
auto4 = Auto "DFH029" [] 0 0 (0,0,0)
--}


