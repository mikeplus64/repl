import Language.Haskell.Repl
import Control.Monad

(-->) = (,)

main :: IO ()
main = do
    repl <- newRepl
    putStrLn "Started repl..."
    let test label ts = do
            putStrLn $ "--- " ++ label ++ " ---"
            mapM_ (\(l,x') -> do x <- prompt repl x'; putStr $ l ++ ": "; mapM_ putStrLn x) ts

    test "Expressions"
        [ "quickly return"              --> "let x = 32 in x"
        , "quickly consume a line"      --> "[0..]"
        , "time out"                    --> "forever (return ()) :: Maybe ()"
        , "time out and show output"    --> "[0,1,2,3,let x = x in x]"
        , "complete quickly and error"  --> "[0,1,2,3,error \"yikes\"]"
        ]

    test "Declarations"
        [ "datatypes"                   --> "data X = X deriving Show"
        , "newtypes"                    --> "newtype X' = X' X"
        , "types"                       --> "type Y = X"
        , "classes"                     --> "class Abc a b | a -> b"
        , "instances"                   --> "instance Abc X X'"
        , "let-bindings"                --> "let x = X; x' = X' x"
        ]

    test "Types"
        [ "x :: X"                      --> ":t x"
        , "fmapfmapfmap"                --> ":t fmap fmap fmap"
        ]

    test "Kinds"
        [ ":k X"                        --> ":k X"
        ]

    test "Misc"
        [ "info"                        --> ":i Monoid"
        , "undefining"                  --> ":d x'"
        , "clear"                       --> ":c"
        , "(try to get something)"      --> ":t X"
        ]
