module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Funcion auxiliar que toma una lista
-- como entrada y devuelve una lista sin repeticiones
unique:: Eq a => [a] -> [a]
unique[] = []
unique(x:xs)
  | elem x xs = xs'
  | otherwise    = x : xs'
  where xs' = unique xs

-- Ejercicio 1
variables :: Prop -> [String]
variables prop = unique (collect_vars prop)
    where 
    collect_vars prop = case prop of
        Var name -> [name]
        Not p -> collect_vars p
        And p q -> (collect_vars p) ++ (collect_vars q)
        Or p q -> (collect_vars p) ++ (collect_vars q)
        Impl p q  -> (collect_vars p) ++ (collect_vars q)
        Syss p q -> (collect_vars p) ++ (collect_vars q)
        Cons _ -> []

-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion p e = case p of
    Var name -> elem name e
    Not p -> not (interpretacion p e)
    And p q -> (interpretacion p e) && (interpretacion q e)
    Or p q -> (interpretacion p e)  || (interpretacion q e)
    Impl p q  -> interpretacion (Or (Not p) q) e
    Syss p q -> interpretacion (And (Impl p q) (Impl q p)) e
    Cons b -> b

-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles p = conjuntoPotencia (variables p)

-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos p = [e | e <- estadosPosibles p, interpretacion p e]

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p q = tautologia (Syss p q)  

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia p = and (map (interpretacion p) (estadosPosibles p))

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica premisas c = tautologia (Impl (andMultiple premisas) c)

-- Función auxiliar que toma una lista de `Prop`s
-- y devuelve el `And` anidado de todas ellas.
andMultiple :: [Prop] -> Prop
andMultiple [] = Cons True
andMultiple (p:ps) = And p (andMultiple ps)

--Funcion auxiliar
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs
