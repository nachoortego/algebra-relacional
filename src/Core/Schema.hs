module Core.Schema (
  Schema(..),
  mkSchema,
  hasAttr,
  schemaAttrs,
  combineSchemas,
  mapSchema,
  intersectSchemas,
  avoidClash
) where

import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Foldable (toList)

-- Se guarda el orden para mejor representación hacia el usuario
data Schema = Schema {
  attrSeq  :: Seq.Seq String,
  attrSet  :: S.Set String
} deriving Show

-- En la igualdad de esquemas no importa el orden
instance Eq Schema where
  s1 == s2 = attrSet s1 == attrSet s2

mkSchema :: [String] -> Schema
mkSchema xs = Schema (Seq.fromList xs) (S.fromList xs)

combineSchemas :: Schema -> Schema -> Schema
combineSchemas s1 s2 = Schema (attrSeq s1 Seq.>< attrSeq s2) (S.union (attrSet s1) (attrSet s2))

intersectSchemas :: Schema -> Schema -> S.Set String
intersectSchemas s1 s2 = S.intersection (attrSet s1) (attrSet s2)

mapSchema :: (String -> String) -> Schema -> Schema
mapSchema f s = Schema (fmap f (attrSeq s)) (S.map f (attrSet s))

hasAttr :: String -> Schema -> Bool
hasAttr a s = S.member a (attrSet s)

schemaAttrs :: Schema -> [String]
schemaAttrs = toList . attrSeq

-- Si k está en avoid, devuelve k_i, donde i es el menor entero tal que k_i no está en avoid.
avoidClash :: S.Set String -> String -> String
avoidClash avoid k
  | S.member k avoid = ac (2 :: Int)
  | otherwise        = k
  where ac i = let candidate = k ++ "_" ++ show i
               in if S.member candidate avoid then ac (i + 1) else candidate