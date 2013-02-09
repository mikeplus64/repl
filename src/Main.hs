import Language.Haskell.Repl
import Control.Monad

main :: IO ()
main = do
    repl <- newRepl
    forever $ print
          =<< prompt repl 
          =<< getLine
