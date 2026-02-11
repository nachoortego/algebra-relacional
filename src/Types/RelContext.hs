module Types.RelContext (Resource(..), Context) where

import qualified Data.Map as M
import Types.Common (Relation)
import Core.AST (RAExp)

-- | Un recurso es una tabla materializada o una vista (expresión).
data Resource = Table Relation | View RAExp
  deriving (Show, Eq)

-- | Entorno: nombres → tablas o vistas.
type Context = M.Map String Resource
