module Main (main) where

import Control.Monad (when)
import System.Directory (doesDirectoryExist)
import System.IO
import qualified Data.Map as M

import Types.RelContext (Context, Resource(..))
import Interface.Repl (repl)
import Utils.LoadCSV (loadCSVsFromDir)

-- | Carga el contexto inicial desde data/ si existe.
-- Si data/ no existe o está vacía, arranca con contexto vacío.
contextoInicial :: IO Context
contextoInicial = do
  exists <- doesDirectoryExist "data"
  if exists
    then do
      (loaded, errs) <- loadCSVsFromDir "data"
      mapM_ putStrLn errs
      return $ foldl (\c (name, rel) -> M.insert name (Table rel) c) M.empty loaded
    else return M.empty

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "=== Álgebra Relacional REPL ==="
  putStrLn "Escribe 'exit' para salir o 'help' para ver las operaciones disponibles"
  ctx <- contextoInicial
  when (not $ M.null ctx) $ putStrLn "Tablas cargadas desde data/"
  repl ctx
