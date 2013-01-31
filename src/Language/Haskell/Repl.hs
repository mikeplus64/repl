{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Repl 
    ( Repl
    -- * Construction
    , newRepl
    , repl'
    , defaultExtensions
    , defaultImports
    , defaultLineLength
    , defaultPatienceForResults
    -- * Stopping
    , stopRepl
    -- * Interaction
    , Input(..)
    , ReplOutput(..)
    , Output(..) 
    , prompt
    , prompt_
    , input
    , output
    , prettyOutput
    , parseInput
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Arrow
import Data.Dynamic
import Data.IORef
import Data.Maybe
import qualified Data.DList as DL
import Text.Parsec hiding (many,(<|>),newline)
import Text.Parsec.String
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H

import GHC
import GHC.Paths
import DynFlags
import GhcMonad
import Outputable (showSDocForUser, Outputable, ppr, neverQualify)

data Input
    = Type     String
    | Kind     String
    | Info     String
    | Decl     String
    | Stmt     String
    | Expr     String
    | Undefine String
    | Clear
  deriving Show

data ReplOutput
    = ReplError String
    | GhcError  String
    | Output    [String]
  deriving Show

data Output
    = OK        [String]
    | Exception [String] String
    | Errors    [String]
    | Partial   [String]
    | Timeout
  deriving Show

prefix :: Char -> Parser ()
prefix c = do
    _ <- string [':',c]
    spaces

input' :: Char -> (String -> Parser a) -> Parser a
input' p f = do
    prefix p
    f =<< getInput

simpl :: Char -> (String -> a) -> Parser a
simpl c f = input' c (return . f)

valid :: (String -> H.ParseResult a) -> String -> Bool
valid f x = case f x of
    H.ParseOk _ -> True
    _           -> False

parseType, parseKind, parseInfo, parseDecl, parseStmt, parseExpr, parseUndefine, parseClear, parseInput :: Parser Input
parseType = simpl 't' Type
parseKind = simpl 'k' Kind
parseInfo = simpl 'i' Info
parseDecl = do
    decl <- getInput
    guard (valid H.parseDecl decl)
    return (Decl decl)
parseStmt = do
    stmt <- getInput
    case H.parseStmt stmt of
        H.ParseOk (H.LetStmt _) -> return (Stmt stmt)
        _                       -> fail "Not a let binding."
parseExpr     = Expr <$> getInput
parseUndefine = simpl 'd' Undefine
parseClear    = simpl 'c' (const Clear)

-- | Used by 'prompt'
parseInput   = foldr1 (\l r -> Text.Parsec.try l <|> r) 
    [ parseClear
    , parseUndefine
    , parseType
    , parseKind
    , parseInfo
    , parseStmt
    , parseDecl
    , parseExpr ]

-- | Used by 'prompt'.
prettyOutput :: Output -> [String]
prettyOutput (OK s)          = s
prettyOutput (Exception s e) = overLast (++ ("*** Exception: " ++ e)) s
prettyOutput (Errors errs)   = errs
prettyOutput (Partial s)     = overLast (++ "*** Timed out") s
prettyOutput Timeout         = ["*** Timed out"]

data Repl = Repl
    { inputChan         :: Chan Input
    , outputChan        :: Chan ReplOutput
    , interpreter       :: ThreadId
    , patienceForResult :: Double
    , lineLength        :: Int
    }

-- | Send input.
input :: Repl -> Input -> IO ()
input = writeChan . inputChan

-- | Naiively get the next set of results. This /does not/ take into account
-- 'patienceForResults', 'patienceForErrors', or 'lineLength'. However, due
-- to laziness, this may not matter.
output :: Repl -> IO ReplOutput
output = readChan . outputChan

{-# INLINE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
ys !? i 
    | i >= 0    = go 0 ys
    | otherwise = Nothing
  where
    go _ [] = Nothing
    go j (x:xs) 
        | j == i    = Just x
        | otherwise = go (j+1) xs

{-# INLINE overLast #-}
overLast :: (a -> a) -> [a] -> [a]
overLast f = go
  where
    go []     = []
    go [x]    = [f x]
    go (x:xs) = x : go xs

-- | Same as 'prompt_', except it parses the input, and pretty prints the results.
prompt
    :: Repl
    -> String
    -> IO [String]
prompt repl x = prettyOutput <$> prompt_ repl (case runParser parseInput () "" x of
    Right a -> a
    -- Should be impossible to reach. parseExpr gobbles up everything.
    _       -> error "Cannot parse input!")

-- | Enter commands and an expression to a 'Repl', and immediately consume results.
-- However, truncate input to the given length, and stop the computation after the
-- given amount of time in seconds.
prompt_ 
    :: Repl
    -> Input
    -> IO Output
prompt_ repl x = do
    input repl x
    results <- output repl
    threads <- newIORef []
    final   <- newEmptyMVar

    -- outputs is used iff an exception is raised by the compiled input.
    outputs <- newIORef [] :: IO (IORef [DL.DList Char])

    let readOutputs = map DL.toList <$> readIORef outputs
        newline     = modifyIORef outputs (++ [DL.empty])
        push char'  = modifyIORef outputs (overLast (`DL.snoc` char'))

        fork f = do
            t <- forkIO $ f `catch` \e@SomeException{} -> do
                outs <- readOutputs
                putMVar final (Exception outs (show e))
            modifyIORef threads (t:)

        -- Get the "progress" of a list.
        prog ys = do
            acc <- newIORef 0
            fork $ forM_ ys $ \_ -> modifyIORef acc (\i -> if i > lineLength repl then i else i+1)
            return acc

    unlessError results $ \ res -> do

        -- Time out
        fork $ do
            threadDelay (floor (patienceForResult repl*1000000))
            u <- readOutputs
            case res !? length u of
                Nothing -> putMVar final (if null u then Timeout else Partial u)
                Just h  -> do
                    p <- prog h
                    i <- readIORef p
                    putMVar final $ case take i h of
                        [] -> case u of
                            [] -> Timeout
                            _  -> Partial u
                        xs -> Partial (u ++ [xs])

        -- Return everything
        fork $ do
            let r = map trim res
            forM_ r $ \l -> do
                newline
                forM_ l push
            putMVar final (OK r)

        fin <- takeMVar final
        mapM_ killThread =<< readIORef threads
        return fin
  where
    trim = take (lineLength repl)

    unlessError (ReplError s) _ = return . Errors . map trim . lines $ s
    unlessError (GhcError  s) _ = return . Errors . map trim . lines $ s
    unlessError (Output s) f    = f s

stopRepl :: Repl -> IO ()
stopRepl = killThread . interpreter

newRepl :: IO Repl
newRepl = do
    inp <- newChan
    out <- newChan
    repl' defaultImports defaultExtensions inp out Nothing Nothing

defaultImports :: [String]
defaultImports
  = ["import Prelude hiding ((.), id, catch)"
    ,"import GHC.TypeLits"
    ,"import qualified Data.Map as M"
    ,"import qualified Data.Foldable as F"
    ,"import qualified Data.Traversable as T"
    ,"import qualified Control.Exception (catch)"
    ,"import Control.Monad.Reader"
    ,"import Control.Monad.State"
    ,"import Control.Monad.Writer"
    ,"import Control.Monad.RWS"
    ,"import Control.Monad.Identity"
    ,"import Control.Monad.ST"
    ,"import Control.Comonad"
    ,"import Control.Category"
    ,"import Control.Monad"
    ,"import Control.Monad.Fix"
    ,"import Control.Applicative"
    ,"import Control.Lens"
    ,"import Control.Arrow"
    ,"import Data.Function hiding ((.), id)"
    ,"import Data.Either"
    ,"import Data.Int"
    ,"import Data.Word"
    ,"import Data.List"
    ,"import Data.Maybe"
    ,"import Data.Semigroup"
    ,"import Data.Bits"
    ,"import Data.Bits.Lens"
    ,"import Data.Ix"
    ,"import Data.Functor"
    ,"import Data.Typeable"
    ]

defaultExtensions :: [ExtensionFlag]
defaultExtensions 
  = [Opt_DataKinds
    ,Opt_PolyKinds
    ,Opt_KindSignatures
    ,Opt_TypeFamilies
    ,Opt_TypeOperators
    ,Opt_DeriveFunctor
    ,Opt_DeriveTraversable
    ,Opt_DeriveFoldable
    ,Opt_DeriveDataTypeable
    ,Opt_DeriveGeneric
    ,Opt_OverloadedStrings
    ,Opt_ImplicitParams
    ,Opt_BangPatterns
    ,Opt_PatternGuards
    ,Opt_MultiWayIf
    ,Opt_LambdaCase
    ,Opt_FlexibleInstances
    ,Opt_FlexibleContexts
    ,Opt_FunctionalDependencies
    ,Opt_GADTs]

defaultLineLength :: Int
defaultLineLength = 512

defaultPatienceForResults :: Double
defaultPatienceForResults = 5

-- | 'Repl' smart constructor.
repl' 
    :: [String]        -- ^ Imports, using normal Haskell import syntax
    -> [ExtensionFlag] -- ^ List of compiler extensions to use
    -> Chan Input      -- ^ Input channel
    -> Chan ReplOutput -- ^ Output channel
    -> Maybe Double    -- ^ Maximum time to wait for a result, in seconds (default: 5)
    -> Maybe Int       -- ^ Maximum line length in 'Char' (default: 512)
    -> IO Repl
repl' imports exts inp out wait len = do
    interp <- forkIO $
        runGhc (Just libdir) $ do
            dflags <- session
            let sdoc :: Outputable a => a -> String
                sdoc = showSDocForUser dflags neverQualify . ppr

                formatType 
                    = splitForAllTys 
                  >>> snd
                  >>> sdoc
                  >>> lines
                  >>> Output

            forever $ do
                import_ imports
                i' <- liftIO (readChan inp)
                liftIO . writeChan out =<< case i' of
                    Clear      -> do
                        setTargets []
                        void (load LoadAllTargets)
                        return $ Output ["OK, I forgot everything."]
                    Undefine _ -> return $ Output ["Not implemented yet."]
                    Type s -> errors $ formatType <$> exprType s
                    Kind s -> errors $ formatType . snd <$> typeKind True s
                    Decl s -> errors $ do _names <- runDecls s; return $ Output ["OK."]
                    Stmt s -> errors $ do void (runStmt s SingleStep); return $ Output ["OK."]
                    Expr s -> errors $ do
                        compiled <- dynCompileExpr $ "show (" ++ s ++ ")"
                        return $ Output [fromDyn compiled ""]
                    Info s -> errors $ do
                        names <- parseName s
                        infos <- concatMap (\(t,f,cs) -> sdoc t : sdoc f : map sdoc cs)
                               . catMaybes
                             <$> mapM getInfo names
                        return $ Output infos
    return $ Repl inp out interp (fromMaybe defaultPatienceForResults wait) (fromMaybe defaultLineLength len)
  where
    errors x = x `gcatch` \ e@SomeException{} -> 
        case fromException e :: Maybe ErrorCall of
            Just _ -> return $ ReplError (show e)
            _      -> return $ GhcError  (show e)

    import_ = mapM (fmap IIDecl . parseImportDecl) >=> setContext
    getExts = foldr (fmap . flip xopt_set) id
    session = do
        s <- getProgramDynFlags
        _ <- setSessionDynFlags 
            $ (\d -> d { safeHaskell = Sf_Safe })
            . flip dopt_set Opt_DoCoreLinting 
            $ getExts exts s

        getSessionDynFlags

