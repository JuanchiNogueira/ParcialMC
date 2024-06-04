module Library where
import PdePreludat


type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String --solo importa el nombre

esVocal :: Char -> Bool
esVocal = flip elem "aeiou"

tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"

cumplen :: (a->b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

riman :: Palabra -> Palabra -> Bool
riman p1 p2 = p1 /= p2 && (rimaAsonante p1 p2 || rimaConsonante p1 p2)

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante = cumplen (ultimasVocales 2) (==)

rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante = cumplen (ultimasLetras 3) (==)

ultimasLetras :: Number -> Palabra -> String
ultimasLetras n = reverse. take n . reverse

ultimasVocales :: Number -> Palabra -> String
ultimasVocales n = ultimasLetras n . filter vocal

vocal :: Char -> Bool
vocal letra = esVocal letra || tieneTilde letra

{-
Las clases de equivalencia son

-Dos palabras riman por rima asonante
-Dos palabras riman por rima consonante
-Dos palabras iguales no riman
-Dos palabras sin conexion no riman

-}

--Conjugaciones

conjugacionRimas :: Verso -> Verso -> Bool
conjugacionRimas v1 v2 = riman (ultimaPalabra v1) (ultimaPalabra v2)

ultimaPalabra :: Verso -> String
ultimaPalabra v1 = drop (length v1 - 1) v1

conjugacionAnadiplosis :: Verso -> Verso -> Bool
conjugacionAnadiplosis v1 v2 = ultimaPalabra v1 == primerPalabra v2

primerPalabra :: Verso -> String
primerPalabra = head.words

--Patrones 

type Patron = Estrofa -> Bool

type Par = (Number , Number)

patronSimple :: Par -> Patron
patronSimple (n1 , n2) estrofa = versoAt n1 estrofa `conjugacionRimas` versoAt n2 estrofa

versoAt :: Number -> Estrofa -> Verso
versoAt n estrofa = estrofa !! (n-1)

esdrujulas :: Patron
esdrujulas = all (esEsdrujula . ultimaPalabra)

esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde . head . ultimasVocales 3

anafora :: Patron
anafora = iguales . map primerPalabra

iguales :: [Palabra] -> Bool
iguales [] = False
iguales (palabra :palabras) = all (== palabra) palabras

type Conjugacion = Verso -> Verso -> Bool

cadena :: Conjugacion -> Patron
cadena _ [] = False
cadena _ [_] = True
cadena conjugacion (verso1:verso2:versos) = conjugacion verso1 verso2 && cadena conjugacion (verso2 : versos)

combinaDos :: Patron -> Patron -> Patron
combinaDos patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa

aabb :: Patron
aabb = patronSimple (1,2) `combinaDos` patronSimple (3,4)

abab :: Patron
abab = patronSimple (1,3) `combinaDos` patronSimple (2,4)

abba :: Patron
abba = patronSimple (1,3) `combinaDos` patronSimple (2,4)


---4

data Escena = UnaEscena {exaltado :: Bool
                        ,potencia :: Number
                        ,freestyle :: Estrofa}

type Estilo = Escena -> Escena

modificarPotencia :: Number -> Estilo
modificarPotencia n escena = escena {potencia = potencia escena + n }

exaltarPublico :: Bool -> Estilo
exaltarPublico exaltado estilo = estilo {exaltado = exaltado}

gritar :: Estilo
gritar = modificarPotencia 50

responderAcote :: Estilo
responderAcote = modificarPotencia 20.exaltarPublico True

exaltarPublicoSiCumple :: Patron -> Escena -> Escena
exaltarPublicoSiCumple patron puesta = exaltarPublico (cumplePatron puesta patron) puesta

cumplePatron :: Escena -> Patron -> Bool
cumplePatron escena patron = patron (freestyle escena)

tirarTecnicas :: Patron -> Estilo
tirarTecnicas patron escena = modificarPotencia 10 (exaltarPublicoSiCumple patron escena)

--5 

type Jurado = [Criterio]

type Criterio = (Escena -> Bool , Number)

puntaje :: Escena -> Jurado -> Number
puntaje puesta = puntajeFinal . valoraciones. criteriosNumberesantes puesta

puntajeFinal :: [Number] -> Number
puntajeFinal = max 3 . sum

valoraciones :: [Criterio] -> [Number]
valoraciones = map snd

criteriosNumberesantes :: Escena -> Jurado -> [Criterio]
criteriosNumberesantes puesta = filter (($ puesta). fst)

doble :: Number -> Number
doble numero = numero + numero


