module Library where
import PdePreludat
import Foreign (Int)
import GHC.Base (Float)
import Test.Hspec (xcontext)

data Participante = Participante {
    nombre :: String,
    trucos :: [Plato->Plato],
    plato :: Plato
} deriving (Show)

data Plato = Plato {
    dificultad :: Number, -- 0-10
    componentes :: [Componente]
} deriving (Show)

{-type Ingrediente = String
type Gramos = Number
type Componente = (Ingrediente,Gramos)-}

data Componente = Componente {
    ingrediente :: String,
    gramos :: Number
} deriving (Show)

-- Parte A
-- Trucos

modificarPlato :: String-> Number -> Plato -> Plato
modificarPlato unIngrediente unosGramos unPlato = unPlato { componentes = componentes unPlato ++ [Componente {ingrediente = unIngrediente, gramos = unosGramos} ]
}

endulzar :: Number -> Plato -> Plato
endulzar gramosAzucar unPlato =  modificarPlato "Azucar" gramosAzucar  unPlato

salar :: Number -> Plato -> Plato
salar gramosSal unPlato = modificarPlato "Sal" gramosSal unPlato

darSabor :: Number -> Number -> Plato -> Plato
darSabor gramosSal gramosAzucar unPlato = (salar gramosSal).endulzar gramosAzucar $ unPlato

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato {componentes = map duplicarGramos (componentes unPlato)}

duplicarGramos :: Componente -> Componente
duplicarGramos unComponente =  unComponente { gramos = gramos unComponente*2}

simplificar :: Plato -> Plato
simplificar unPlato 
    | masDeComponentes 5 unPlato && dificultadMayorA 7 unPlato = (modificarDificultad 5).(limpiarComponentesMenoresA 10) $ unPlato
    | otherwise = unPlato

masDeComponentes :: Number -> Plato -> Bool 
masDeComponentes unNumero unPlato = length (componentes unPlato) > unNumero

dificultadMayorA :: Number -> Plato -> Bool  
dificultadMayorA unNumero unPlato = (dificultad unPlato) > unNumero 

modificarDificultad :: Number -> Plato -> Plato
modificarDificultad unNumero unPlato = unPlato{ dificultad = unNumero} 

limpiarComponentesMenoresA :: Number -> Plato -> Plato
limpiarComponentesMenoresA unNumero unPlato = unPlato{componentes = filter (mayoresA unNumero) (componentes unPlato)}

mayoresA :: Number ->  Componente -> Bool
mayoresA unNumero unComponente = gramos unComponente > unNumero

-- Info de los platos

esVegano :: Plato -> Bool
esVegano unPlato = not(contieneElIngrediente "Carne" (componentes unPlato) ) && not(contieneElIngrediente "Huevo" (componentes unPlato) ) && not(tieneAlimentosLacteos (componentes unPlato))

contieneElIngrediente :: String -> [Componente] -> Bool
contieneElIngrediente unIngrediente listaComponentes = (elem unIngrediente).(map ingrediente) $ listaComponentes

listaLacteos :: [String]
listaLacteos = ["Leche", "Manteca", "Crema"]

tieneAlimentosLacteos :: [Componente] -> Bool
tieneAlimentosLacteos listaComponentes = any (\unComponente -> elem (ingrediente unComponente) listaLacteos) listaComponentes

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not(contieneElIngrediente "Harina" (componentes unPlato))

esComplejo :: Plato -> Bool
esComplejo unPlato = masDeComponentes 5 unPlato && dificultadMayorA 7 unPlato

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = not(null.(filter platoSalado)$(componentes unPlato))

platoSalado :: Componente -> Bool
platoSalado unComponente = ingrediente unComponente == "Sal" &&  gramos unComponente > 2

-- Parte B
salPepe :: Componente
salPepe = Componente {ingrediente = "Sal", gramos = 2}
pimientaPepe :: Componente
pimientaPepe = Componente {ingrediente = "Pimienta", gramos = 1}
harianaPepe :: Componente
harianaPepe = Componente {ingrediente = "Harina", gramos = 400}
huevoPepe :: Componente
huevoPepe = Componente {ingrediente = "Huevo", gramos = 2}
pimentonPepe :: Componente
pimentonPepe = Componente {ingrediente = "Pimenton", gramos = 2}
aceitePepe :: Componente
aceitePepe = Componente {ingrediente = "Aceite", gramos = 2}

platoPepe :: Plato
platoPepe = Plato { dificultad = 8, componentes = [salPepe,pimientaPepe,harianaPepe,huevoPepe,pimentonPepe,aceitePepe]}

pepeRoccino :: Participante
pepeRoccino = Participante { nombre = "Pepe Roccino", trucos = [darSabor 2 5, simplificar, duplicarPorcion], plato = platoPepe }   
 
-- Parte C

cocinar :: Participante -> Participante
cocinar unParticipante = unParticipante {plato = foldl (\ unPlato unTruco -> unTruco unPlato)(plato unParticipante) (trucos unParticipante)}

-- plato1 es mejor que plato2
esMejorQue :: Participante -> Participante -> Bool
esMejorQue participante1 participante2 = mayorDificultad (plato participante1) (plato participante2) && menorPesos (plato participante1) (plato participante2) 

mayorDificultad :: Plato -> Plato -> Bool
mayorDificultad plato1 plato2 = dificultad plato1 > dificultad plato2

menorPesos :: Plato -> Plato -> Bool
menorPesos plato1 plato2  = sumaDePesosComponente (componentes plato1) < sumaDePesosComponente (componentes plato2)

sumaDePesosComponente :: [Componente] -> Number
sumaDePesosComponente listaComponentes = sum.(map gramos) $ listaComponentes

-- Asumo que siempre va a tener al menos un participante
participanteEstrella :: [Participante] -> Participante
participanteEstrella [x] = cocinar x
participanteEstrella (x:xs) 
    |  esMejorQue (cocinar x) (participanteEstrella xs) = cocinar x
    | otherwise = participanteEstrella xs


-- Parte D 

infinitosComponentesMisteriosos :: [Componente]
infinitosComponentesMisteriosos = [Componente { ingrediente = ("Ingrediente " ++ show n), gramos = n } | n <- [1..]]

platinum = Plato {
    dificultad=10, 
    componentes = infinitosComponentesMisteriosos}

