module Types.Common (Value(..), Tuple, Relation(..), valToString) where

import qualified Data.Map as M
import qualified Data.Set as S
import Core.Schema

-- 1. VALUES
-- Usamos un ADT (Algebraic Data Type) para envolver los tipos primitivos.
-- Derivamos Eq y Ord porque necesitamos saber si dos tuplas son iguales y ordenarlas en el Set.
data Value = VInt Int | VStr String 
  deriving (Eq, Ord)

-- Definimos cómo se muestra un valor por pantalla (para debugging o impresión final)
instance Show Value where
  show (VInt n) = show n
  show (VStr s) = show s

valToString :: Value -> String
valToString (VInt n) = show n
valToString (VStr s) = s

-- 2. TUPLE
-- Una tupla es un diccionario: Atributo -> Valor
type Tuple = M.Map String Value

-- 3. RELATIONS
-- Una relación consta de un esquema (lista ordenada de atributos)
-- y un cuerpo (conjunto de tuplas).
data Relation = Rel {
  attributes :: Schema,      -- El esquema, ordenado y eficiente
  tuples     :: S.Set Tuple  -- El contenido, sin duplicados.
} deriving (Eq, Show)
