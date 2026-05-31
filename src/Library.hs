module Library where
import PdePreludat

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
mayoresA unNumero unComponente = gramos unComponente >= unNumero

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


 -- Platos y participantes para pruebas 

platoTest :: Plato
platoTest = Plato { dificultad = 6 , componentes = [Componente { ingrediente = "Carne", gramos = 200 }, Componente { ingrediente = "Harina", gramos = 50 }]}

platoDificil :: Plato
platoDificil = Plato { dificultad = 9  , componentes= [Componente { ingrediente = "A", gramos = 5 }, Componente { ingrediente = "B", gramos = 5 }, 
Componente { ingrediente = "C", gramos = 10} , Componente { ingrediente = "D", gramos = 5 },
Componente{ingrediente = "D", gramos = 5 }, Componente{ingrediente = "E", gramos = 5 }, Componente{ingrediente = "F", gramos = 11 }]}

participante1 :: Participante
participante1= Participante { nombre = "Pepe", trucos = [endulzar 5, salar 2], plato = platoTest }

plato2 :: Plato
plato2 = Plato { dificultad = 10, componentes = [Componente { ingrediente = "Lechuga", gramos = 50 }]}

participante2 :: Participante
participante2 = Participante { nombre = "Ana", trucos = [salar 1], plato = plato2 }

platoEnsalada :: Plato
platoEnsalada = Plato { dificultad = 4 , componentes= [Componente { ingrediente = "Lechuga", gramos = 100 }, Componente { ingrediente = "Tomate", gramos = 50 }]}

platoTarta :: Plato
platoTarta = Plato { dificultad = 8, componentes = [Componente { ingrediente = "Queso", gramos = 150 }, Componente {ingrediente= "Queso", gramos= 100}]}

participante3 :: Participante
participante3 = Participante {nombre = "Juana", trucos = [salar 1] , plato = platoEnsalada}

participante4 :: Participante
participante4 = Participante{ nombre= "Carlos" , trucos = [duplicarPorcion, endulzar 2], plato= platoTarta}

listaParticipantes :: [Participante]
listaParticipantes = [pepeRoccino, participante1,participante3,participante4]