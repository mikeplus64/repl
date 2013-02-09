import Language.Haskell.Repl
import Control.Monad
import System.Environment

help :: String
help = "Usage: repl [-h|-s]\n\tWhen '-s' or '--use-show' is the argument, repl will call print on the output. This makes it much easier for applications interpretting the output to know when something starts and ends. A Haskell application can just use read :: String -> [String]."

main :: IO ()
main = do
    repl <- newRepl
    args <- getArgs
    case args of
        ["-h"]         -> putStrLn help
        ["--help"]     -> putStrLn help
        ["-s"]         -> forever $ print =<< prompt repl =<< getLine
        ["--use-show"] -> forever $ print =<< prompt repl =<< getLine
        _              -> forever $ mapM_ putStrLn =<< prompt repl =<< getLine
