import Lexer
import Parser
import CMips
import Interm
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  txt <- getContents
  let funs = parser (alexScanTokens txt)
  let code = evalState (interm Map.empty funs) (0,0,0,0)
  mips code
