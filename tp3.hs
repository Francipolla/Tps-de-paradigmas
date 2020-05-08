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

type Accion = (String, Participante -> Participante)

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista"        [] [("Pasar por el banco", pasarPorElBanco) , ("Pagar a accionistas" , pagarAAccionistas)]
manuel   :: Participante 
manuel   = UnParticipante "Manuel"   500 "Oferente singular" [] [("Pasar por el banco" , pasarPorElBanco)  , ("Enojarse" , enojarse)]

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

pasarPorElBanco :: Participante -> Participante
pasarPorElBanco unJugador = ((cambiarTactica "Comprador Compulsivo").(agregarDinero 40)) unJugador

cambiarTactica  :: String -> Participante -> Participante
cambiarTactica nuevaTactica unJugador = unJugador { tacticaDeJuego = nuevaTactica}

agregarDinero   :: Int -> Participante -> Participante
agregarDinero dinero unJugador = unJugador { cantidadDeDinero = (cantidadDeDinero unJugador) + dinero}

pagarAAccionistas :: Participante -> Participante
pagarAAccionistas unJugador | (tacticaDeJuego unJugador) == "Accionista" = agregarDinero 200 unJugador | otherwise = restarDinero 100 unJugador

restarDinero   :: Int -> Participante -> Participante
restarDinero dinero unJugador = unJugador { cantidadDeDinero = (cantidadDeDinero unJugador) - dinero}

enojarse :: Participante -> Participante
enojarse unJugador =  ((agregarDinero 50).(gritar (++"AHHHH"))) unJugador

gritar :: (String -> String) -> Participante -> Participante 
gritar grito unJugador = unJugador { nombre = grito (nombre unJugador)}

agregarAccion :: Accion -> Participante -> Participante
agregarAccion accion unJugador = unJugador {accionesARealizar = accion : (accionesARealizar unJugador)}

subastar :: Participante -> Propiedad -> Participante
subastar unJugador propiedad | tacticaAceptable unJugador = unJugador {propiedadesCompradas = propiedad : (propiedadesCompradas unJugador)}
                             | otherwise = unJugador

tacticaAceptable   :: Participante -> Bool
tacticaAceptable unJugador = (tacticaDeJuego unJugador) == "Oferente singular" || (tacticaDeJuego unJugador) == "Accionista"

cobrarAlquiler     :: Participante -> Participante
cobrarAlquiler unJugador = unJugador {cantidadDeDinero = (cantidadDeDinero unJugador) + (10*((length.propiedadesBaratas) unJugador)) + (20*((length.propiedadesCaras) unJugador))}

propiedadesBaratas :: Participante -> [Int]
propiedadesBaratas unJugador = ((filter (<150)).listaDePrecios) unJugador

propiedadesCaras   :: Participante -> [Int]
propiedadesCaras   unJugador = ((filter (>=150)).listaDePrecios) unJugador

listaDePrecios :: Participante -> [Int]
listaDePrecios unJugador = map precioPropiedad (propiedadesCompradas unJugador)

precioPropiedad :: Propiedad -> Int
precioPropiedad (a,b) = b

realizarAcciones :: Participante -> [Participante]
realizarAcciones unJugador = map (\unaAccion -> unaAccion unJugador) (listaDeAcciones unJugador)

listaDeAcciones :: Participante -> [Participante -> Participante]
listaDeAcciones unJugador = map accionDeJugador (accionesARealizar unJugador)

accionDeJugador :: Accion -> (Participante -> Participante)
accionDeJugador (a,b) = b