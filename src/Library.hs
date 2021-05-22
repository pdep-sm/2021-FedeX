module Library where
import PdePreludat

-- Punto 1
type Ciudad = String
type Pais = String
type Lugar = (Ciudad, Pais)
type Categoria = String
type Peso = Number
type Precio = Number
type Cargo = Envio -> Envio
type Impuesto = Envio -> Precio
data Envio = 
    Envio {
        origen :: Lugar,
        destino :: Lugar,
        peso :: Peso,
        precioBase :: Precio,
        categorias :: [Categoria],
        impuestos :: [Impuesto]
    }

-- Punto 2a
cargo :: (Envio -> Bool) -> (Envio -> Number) -> Cargo
cargo criterio calculo envio 
    | not . criterio $ envio = envio
    | criterio envio = envio { precioBase = precioBase envio + calculo envio }

cargoCategorico :: Categoria -> Number -> Cargo
cargoCategorico categoria factor = cargo (elem categoria.categorias) ((factor *).precioBase)

cargoTecnologico :: Cargo
cargoTecnologico = cargo (elem "tecnología".categorias) ((*0.18).precioBase)
cargoTecnologico' = cargoCategorico "tecnología" 0.18

cargoSobrepeso :: Peso -> Cargo
cargoSobrepeso maximo = cargo ((>maximo).peso) ((*80).(flip (-) maximo).peso)
cargoSobrepeso' maximo = cargo (\ _ -> True) ((*80). max 0 .(flip (-) maximo).peso)
cargoSobrepeso'' maximo = cargo (const True) ((*80). max 0 .(flip (-) maximo).peso)

cargoArbitrario :: Cargo
cargoArbitrario = cargo (const True) (const 50)

-- Impuestos
impuesto :: (Envio -> Bool) -> (Precio -> Number) -> Impuesto
impuesto criterio calculo envio 
    | not . criterio $ envio = 0
    | criterio envio = calculo $ precioBase envio

iva = impuesto (const True) (*0.2)
multicategoria = impuesto ((>3).length.categorias) (*0.01)
aduanero = impuesto esInternacional (*0.03)
extranio = impuesto (even.precioBase) (*0.1)

-- Punto 2b
envioInternacional = Envio {
        origen = ("Buenos Aires", "Argentina"),
        destino = ("Utrecht", "Paises Bajos"),
        peso = 2,
        precioBase = 220,
        categorias = ["música","tecnología"],
        impuestos = []
}

-- Punto 2c
envioDomestico = Envio 
    ("California", "Estados Unidos") 
    ("Miami", "Estados Unidos") 
    5 
    1500 
    ["libros"]
    [iva, extranio]

-- Punto 3

-- Punto 4a
seDirige unPais = (unPais==).pais.destino
-- Punto 4b
pais = snd
esLocal envio = seDirige (pais.origen $ envio) envio
esInternacional = not.esLocal

-- Punto 6 ( se recomienda hacer :D )

-- Punto 8
whatever a b c  = c a . filter ((a==) . b)