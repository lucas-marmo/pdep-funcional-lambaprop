module Library where
import PdePreludat

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = UnDepto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = UnaPersona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun :: (a->a->Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a->a->a->Bool
between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  UnDepto 3 80 7500 "Palermo", 
  UnDepto 1 45 3500 "Villa Urquiza", 
  UnDepto 2 50 5000 "Palermo", 
  UnDepto 1 45 5500 "Recoleta"]

depto1 = UnDepto 3 80 7500 "Palermo"
depto2 = UnDepto 1 45 3500 "Villa Urquiza" 
depto3 = UnDepto 2 50 5000 "Palermo" 
depto4 = UnDepto 1 45 5500 "Recoleta"
{----------------------------------------------PUNTO 1----------------------------------------------}
mayor :: Ord a => (b->a) -> b -> b -> Bool
mayor func valor1 valor2 = func valor1 > func valor2

menor :: Ord a => (b->a) -> b -> b -> Bool
menor func valor1 = not.mayor func valor1

ordenarSegunLength :: [String] -> [String]
ordenarSegunLength listaStrings = ordenarSegun (mayor length) listaStrings
ordenarSegunLength' listaStrings = ordenarSegun (menor length) listaStrings 

{----------------------------------------------PUNTO 2----------------------------------------------}
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barrios departamento = elem (barrio departamento) barrios

cumpleRango :: (Depto -> Number) -> Number -> Number -> Requisito
cumpleRango func valor1 valor2 departamento = between valor1 valor2 (func departamento)

{----------------------------------------------PUNTO 3----------------------------------------------}
cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda requisitos depto = all ($ depto) requisitos

buscar :: Busqueda -> (Depto->Depto->Bool) -> [Depto] -> [Depto]
buscar requisitos criterio = ordenarSegun criterio.filter (cumpleBusqueda requisitos)
--buscar [ubicadoEn ["Palermo","Recoleta"],cumpleRango ambientes 1 2,cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo

{----------------------------------------------PUNTO 4----------------------------------------------}
mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto personas = ((map mail).filter (algunaBusquedaCoincide depto)) personas

algunaBusquedaCoincide :: Depto -> Persona -> Bool
algunaBusquedaCoincide depto persona = any (flip cumpleBusqueda depto) (busquedas persona)

persona1 = UnaPersona "persona1@gmail.com" [[ubicadoEn ["Palermo","Recoleta"],cumpleRango ambientes 1 2,cumpleRango precio 0 6000]]