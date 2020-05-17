import Text.Show.Functions

main :: IO ()
main = return ()

------------------ Participantes ------------------

data Participante = UnParticipante {
nombre               :: String,
cantidadDeDinero     :: Int,
tacticaDeJuego       :: String,
propiedadesCompradas :: [Propiedad],
accionesARealizar    :: [Accion]
} deriving(Show)

type Accion = Participante -> Participante

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista"        [] [ pasarPorElBanco, pagarAAccionistas]
manuel   :: Participante 
manuel   = UnParticipante "Manuel"   500 "Oferente singular" [] [ pasarPorElBanco, enojarse]

------------------ Propiedades --------------------
type Propiedad = (String, Int)

casaBlanca              :: Propiedad
casaBlanca              = ("Casa Blanca", 50)

laBombonera             :: Propiedad
laBombonera             = ("La Bombonera", 1000)

estadioPokemon          :: Propiedad
estadioPokemon          = ("Estadio Pokemon", 300)

loDeCarlos              :: Propiedad
loDeCarlos              = ("Lo de Carlos", 20)

residenciaEscobar       :: Propiedad
residenciaEscobar       = ("Residencia Escobar", 500)

centralPerk             :: Propiedad
centralPerk             = ("Central Perk", 90)

------------------ Acciones -----------------------

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = ((cambiarTactica "Comprador Compulsivo").(agregarDinero 40)) unJugador

cambiarTactica  :: String -> Accion
cambiarTactica nuevaTactica unJugador = unJugador { tacticaDeJuego = nuevaTactica}

agregarDinero   :: Int -> Accion
agregarDinero dinero unJugador = unJugador { cantidadDeDinero = (cantidadDeDinero unJugador) + dinero}

pagarAAccionistas :: Accion
pagarAAccionistas unJugador | (tacticaDeJuego unJugador) == "Accionista" = agregarDinero 200 unJugador | otherwise = restarDinero 100 unJugador

restarDinero   :: Int -> Accion
restarDinero dinero unJugador = unJugador { cantidadDeDinero = (cantidadDeDinero unJugador) - dinero}

enojarse :: Accion
enojarse unJugador =  ((agregarDinero 50).gritar) unJugador

gritar :: Accion
gritar unJugador = unJugador { nombre = ("AHHHH"++)(nombre unJugador)}

agregarAccion :: Accion -> Accion
agregarAccion accion unJugador = unJugador {accionesARealizar = accion : (accionesARealizar unJugador)}

subastar :: Participante -> Propiedad -> Participante
subastar unJugador propiedad | tacticaAceptable unJugador = (restarDinero (precioPropiedad propiedad)) (unJugador {propiedadesCompradas = propiedad : (propiedadesCompradas unJugador)}) 
                             | otherwise = unJugador

tacticaAceptable   :: Participante -> Bool
tacticaAceptable unJugador = (tacticaDeJuego unJugador) == "Oferente singular" || (tacticaDeJuego unJugador) == "Accionista"

cobrarAlquiler     :: Accion
cobrarAlquiler unJugador = agregarDinero ((10*((length.propiedadesBaratas) unJugador)) + (20*((length.propiedadesCaras) unJugador))) unJugador

esBarata :: Int -> Bool
esBarata = (<150)

propiedadesBaratas :: Participante -> [Int]
propiedadesBaratas unJugador = ((filter esBarata).listaDePrecios) unJugador

propiedadesCaras   :: Participante -> [Int]
propiedadesCaras   unJugador = ((filter (not.esBarata)).listaDePrecios) unJugador

listaDePrecios :: Participante -> [Int]
listaDePrecios unJugador =((map precioPropiedad).propiedadesCompradas) unJugador

precioPropiedad :: Propiedad -> Int
precioPropiedad (a,b) = b

ultimaRonda :: Participante -> Accion
ultimaRonda unJugador = foldr1 (.) (accionesARealizar unJugador)

juegoFinal :: Participante -> Participante -> String 
juegoFinal participante1 participante2 | (cantidadDeDinero ((ultimaRonda participante1) participante1)) < (cantidadDeDinero ((ultimaRonda participante2) participante2))  = ("Ganador " ++ )(nombre participante2)
                                       | otherwise = ("Ganador " ++)(nombre participante1)


hacerBerrinche :: Propiedad -> Accion
hacerBerrinche propiedad unJugador | (cantidadDeDinero unJugador) < (precioPropiedad propiedad) = hacerBerrinche propiedad (gritar.(agregarDinero 10) $ unJugador)
                                   | otherwise                                                  = (restarDinero (precioPropiedad propiedad)) (unJugador {propiedadesCompradas = propiedad : (propiedadesCompradas unJugador)})                                  


francisco :: Participante
francisco = UnParticipante "Francisco" 0 "DaleBoo"        [] []                                    