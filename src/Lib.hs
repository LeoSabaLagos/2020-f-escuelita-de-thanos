module Lib where
import Text.Show.Functions

laVerdad = True

-- Escuelita de Thanos 

{- 
Primera parte
Los enanos de Nidavellir nos han pedido modelar los guanteletes que ellos producen en su herrería. 

Un guantelete está hecho de un material (“hierro”, “uru”, etc.) y sabemos las gemas que posee. 
También se sabe de los personajes que tienen una edad, una energía, 
una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc), 
su nombre y en qué planeta viven. 

Los fabricantes determinaron que cuando un guantelete está completo -es decir, 
tiene las 6 gemas posibles- y su material es “uru”, se tiene la posibilidad de chasquear 
un universo que contiene a todos sus habitantes y reducir a la mitad la cantidad de dichos personajes. 

Por ejemplo si tenemos un universo en el cual existen ironMan, drStrange, groot y wolverine, 
solo quedan los dos primeros que son ironMan y drStrange. 

Si además de los 4 personajes estuviera viudaNegra, quedarían también ironMan 
y drStrange porque se considera la división entera.
 -}

--------------------------------------------------------------------------
-- Punto 1
-- Modelar Personaje, Guantelete y Universo como tipos de dato e implementar el chasquido de un universo.

type Material = String 

data Guantelete = Guantelete{
    material :: Material,
    gemas :: [Gema]
}deriving(Show)

type Nombre = String
type Edad = Int 
type Energia = Int
type Habilidad = String
type Planeta = String

data Personaje = Personaje{
    nombre :: Nombre,
    edad :: Edad,
    energia :: Energia,
    habilidades :: [Habilidad],
    planeta :: Planeta
}deriving(Show,Eq)

data Universo = Universo{ habitantes :: [Personaje] } deriving(Show,Eq)

guanteleteCompleto = Guantelete "uru" [tiempo,espacio "Venus"]
guanteleteVacio = Guantelete "hierro" [poder,alma "volar"]
guanteleteUruguayo = Guantelete "uru" [gemaLoca tiempo]

estaCompleto :: Guantelete -> Bool
estaCompleto guantelete = tieneSeisGemas guantelete && esDeUru guantelete

tieneSeisGemas :: Guantelete -> Bool
tieneSeisGemas = (==6).length.gemas

esDeUru :: Guantelete -> Bool
esDeUru = (=="uru").material

chasquido :: Universo -> Guantelete -> Universo
chasquido universo guantelete 
    | estaCompleto guantelete = reducirALaMitad universo
    | otherwise = universo

reducirALaMitad :: Universo -> Universo 
reducirALaMitad universo = universo{habitantes = mitadDe (habitantes universo)}

mitadDe :: [Personaje] -> [Personaje]
mitadDe habitantes = take (divididoPorDos (length habitantes)) habitantes

type CantidadHabitantes = Int

divididoPorDos :: CantidadHabitantes -> CantidadHabitantes
divididoPorDos cantidadHabitantes = cantidadHabitantes `div` 2 

viaLactea = Universo [ironMan,drStrange,groot,wolverine,viudaNegra]

ironMan = Personaje "Iron Man" 45 1000 ["rayo laser","bola de energia","volar"] "Tierra"
drStrange = Personaje "Dr.Strange" 100 1000 ["Portal","Escudo energetico"] "Tierra"
groot = Personaje "Groot" 30 100 ["Enredadera"] "Tierra"
wolverine = Personaje "Wolverine" 50 1000 ["Rasguño","Revivir"] "Tierra"
viudaNegra = Personaje "Viuda Negra" 30 100 ["Tiro Letal","Patada Descendente"] "Tierra"
--------------------------------------------------------------------------
-- Punto 2
-- analizarUniverso ??

aptoParaPendex :: (Personaje -> Bool) -> Universo -> Bool
aptoParaPendex criterioPendex = (any criterioPendex).habitantes

menosDeCuarenticincoAnios :: Personaje -> Bool
menosDeCuarenticincoAnios = (<45).edad

energiaTotal :: Universo -> Energia
energiaTotal = sum.obtenerEnergias.habitantes

obtenerEnergias :: [Personaje] -> [Energia]
obtenerEnergias = map energia 
--------------------------------------------------------------------------
-- Punto 3
type Gema = Personaje -> Personaje 

-- La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
mente :: Energia -> Gema
mente energiaDenegada personaje = reducirEnergia energiaDenegada personaje

reducirEnergia :: Energia -> Personaje -> Personaje
reducirEnergia energiaDenegada personaje = 
    personaje{energia = (max 0 ( (energia personaje) - energiaDenegada) )}


-- El alma puede controlar el alma de nuestro oponente permitiéndole 
-- eliminar una habilidad en particular si es que la posee. 
-- Además le quita 10 puntos de energía. 

alma :: Habilidad -> Gema
--alma habilidadAQuitar personaje = reducirEnergia 10 (quitarHabilidad habilidadAQuitar personaje)
alma habilidadAQuitar = (reducirEnergia 10).(quitarHabilidad habilidadAQuitar)

quitarHabilidad :: Habilidad -> Personaje -> Personaje
quitarHabilidad habilidadAQuitar personaje 
    | elem habilidadAQuitar (habilidades personaje) = personaje{habilidades = sacarHabilidad habilidadAQuitar personaje}
    | otherwise = personaje

sacarHabilidad :: Habilidad -> Personaje -> [Habilidad]
sacarHabilidad habilidadAQuitar = (filter (/= habilidadAQuitar)).habilidades 

-- El espacio que permite transportar al rival al planeta x (el que usted decida) 
-- y resta 20 puntos de energía.

espacio :: Planeta -> Gema
espacio planetaDestino = (reducirEnergia 20).(transportarPersonaje planetaDestino)

transportarPersonaje :: Planeta -> Personaje -> Personaje 
transportarPersonaje planetaDestino personaje = personaje{planeta = planetaDestino}

-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita 
-- (en caso contrario no le saca ninguna habilidad).

poder :: Gema
poder personaje = reducirEnergia (energia personaje) (dejarSinHabilidades personaje)
--poder personaje = ((reducirEnergia (energia personaje)).dejarSinHabilidades) personaje

dejarSinHabilidades :: Personaje -> Personaje
dejarSinHabilidades personaje
    | length (habilidades personaje) <= 2 = personaje{habilidades = []}
    | otherwise = personaje

{-
El tiempo que reduce a la mitad la edad de su oponente pero como no está permitido pelear con menores, 
no puede dejar la edad del oponente con menos de 18 años. 

Considerar la mitad entera, por ej: si el oponente tiene 50 años, le quedarán 25. 
Si tiene 45, le quedarán 22 (por división entera). 

Si tiene 30 años, le deben quedar 18 en lugar de 15. 

También resta 50 puntos de energía.
-}

tiempo :: Gema 
tiempo = (reducirEnergia 50).mitadDeEdad

mitadDeEdad :: Personaje -> Personaje
mitadDeEdad personaje = personaje{edad = max 18 (div (edad personaje) 2)}

-- La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

ejercerGema :: Gema -> Personaje -> Personaje
ejercerGema gema = gema
--------------------------------------------------------------------------
-- Punto 4
guanteleteGoma = Guantelete "goma" [tiempo,alma "usar Mjolnir",gemaLoca (alma "programacion en Haskell")]
--------------------------------------------------------------------------
-- Punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gema personaje = foldr ejercerGema personaje gema
--------------------------------------------------------------------------
-- Punto 6

gemaMasPoderosa ::  Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje = gemaMasPoderosaDelGuantelete personaje.gemas

gemaMasPoderosaDelGuantelete :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDelGuantelete _ [gema] = gema
gemaMasPoderosaDelGuantelete personaje (gema1:gema2:colaGemas) 
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDelGuantelete personaje (gema1:colaGemas) 
    | otherwise = gemaMasPoderosaDelGuantelete personaje (gema2:colaGemas)
--------------------------------------------------------------------------
-- Punto 7

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete


{- utilizar :: Personaje -> [Gema] -> Personaje
utilizar personaje = foldr ejercerGema personaje -}

{- gemaMasPoderosa ::  Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje = gemaMasPoderosaDelGuantelete personaje.gemas -}

--gemaMasPoderosa punisher guanteleteDeLocos
{- 
Esto se podria ejecutar, pero nunca terminaria de evaluar cual seria la gema mas poderosa
ya que se tienen que ir evaluando de a pares las gemas que son de una cantidad infinita
Haskell al evaluar con el lazy evaluation seguiria evaluando infinitamente la lista de gemas
del guanteleteDeLocos 
-}

--usoLasTresPrimerasGemas guanteleteDeLocos punisher
{-
Esto se podria ejecutar, y ademas devolveria el personaje habiendole aplicado las 3 gemas
de ejectuar utilizar ya que la funcion take 3 agarraria 
los primeros 3 elementos de la lista infinita 
de gemas del guantelete gracias al lazy evaluation
-}

--------------------------------------------------------------------------
