import Language.Haskell.Repl

(-->) :: a -> b -> (a,b)
(-->) = (,)

main :: IO ()
main = do
    repl <- newRepl
    putStrLn "Started repl..."
    let test label ts = do
            putStrLn $ "--- " ++ label ++ " ---"
            mapM_ 
                (\(l,x') -> do 
                    x <- prompt repl x'
                    putStr $ l ++ ": "
                    mapM_ putStrLn x) 
                ts

    test "Expressions"
        [ "quickly return"              --> "let x = 32 in x"
        , "quickly consume a line"      --> "[0..]"
        , "time out"                    --> "forever (return ()) :: Maybe ()"
        , "time out and show output"    --> "[0,1,2,3,let x = x in x]"
        , "complete quickly and error"  --> "[0,1,2,3,error \"yikes\"]"
        , "unicode string"              --> "let (⧺) = (++) in \"aaaa\" ⧺  \"私はバンゴホルーです。\" :: String"
        ]

    test "Declarations"
        [ "datatypes"                   --> "data X = X deriving Show"
        , "newtypes"                    --> "newtype X' = X' X"
        , "types"                       --> "type Y = X"
        , "classes"                     --> "class Abc a b | a -> b"
        , "instances"                   --> "instance Abc X X'"
        , "let-bindings"                --> "let x = X; x' = X' x"
        , "normal binding (should fail)"--> "asdf = 31" 
        , "unicode let binding"         --> "let あ = 'a'"
        ]

    test "Types"
        [ "x :: X"                      --> ":t x"
        , ":t あ"                       --> ":t あ"
        , "fmapfmapfmap"                --> ":t fmap fmap fmap"
        ]

    test "Kinds"
        [ ":k X"                        --> ":k X"
        , ":k Abc"                      --> ":k Abc"
        ]

    test "Misc"
        [ "info"                        --> ":i Monoid"
        , "undefining x"                --> ":d x"
        , "try to get x anyway"         --> ":t x"
        , "clear"                       --> ":c"
        , "try to get X anyway"         --> ":t X"
        ]
