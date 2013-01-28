import Language.Haskell.Repl
import Control.Monad
main :: IO ()
main = do
    r <- newRepl
    forever $ do
        l <- getLine
        promptWith r putStrLn [] l
