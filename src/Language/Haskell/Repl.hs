module Language.Haskell.Repl where

import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Arrow
import Data.Dynamic
import GHC
import GHC.Paths (libdir)
import DynFlags
import GhcMonad
import Outputable (showSDocForUser, ppr, neverQualify, Outputable)
import Data.IORef
import Data.Maybe
import Data.Foldable (for_)

data Input
    = Type      { input :: String }
    | Kind      { input :: String }
    | Info      { input :: String }
    | Decl      { input :: String }
    | Bind      { input :: String }
    | Expr      { input :: String }
    | Undefine  { input :: String }
    | Clear
  deriving Show

data Output
    = ReplError { failure :: String }
    | GhcError  { failure :: String }
    | Output    { output :: [String] }
  deriving Show

data Repl = Repl
    { inputChan         :: Chan Input
    , outputChan        :: Chan Output
    , interpreter       :: ThreadId
    , patienceForResult :: Maybe Double
    , patienceForErrors :: Maybe Int
    , lineLength        :: Maybe Int
    }

{-
enter :: Repl 
      -> Input
      -> IO ()
enter r x = writeChan (input r) x

-- | Naiively get the next set of results. This /does not/ take into account
-- 'patienceForResults', 'patienceForErrors', or 'lineLength'.
results :: Repl -> IO [String]
results = readChan . output

-- | Enter commands and an expression to a 'Repl', and immediately consume results.
-- However, truncate input to the given length, and stop the computation after the
-- given amount of time in seconds.
prompt 
    :: Repl
    -> Input
    -> IO [String]
prompt r xs x = do
    let trimLine = case lineLength r of
            Just l -> take l
            _      -> id
    enter r xs x
    lazyResults <- results r
    final       <- newEmptyMVar
    timeout     <- forkIO (patienceForResult r `for_` \ p -> do
        threadDelay (floor (p*1000000))
        putMVar final ["Thread timed out."])
    attempt <- forkIO $ case patienceForErrors r of
        Just p -> do
            (tr,ir)  <- progress lazyResults
            threadDelay p
            killThread tr
            prog <- readIORef ir
            let hs = map trimLine (take prog lazyResults)
            ends hs `seq` putMVar final hs                

        _ -> putMVar final (map trimLine lazyResults)

    fin <- takeMVar final
    killThread timeout
    killThread attempt
    return fin

promptWith
    :: Repl
    -> (String -> IO a)
    -> [String]
    -> String
    -> IO ()
promptWith r f xs x = prompt r xs x >>= mapM_ f
-}

-- | See if a lazy list has ended.
ends :: [a] -> Bool
ends []     = True
ends (_:xs) = ends xs

-- | See 'how far' a lazy list has evaluated.
progress :: [a] -> IO (ThreadId, IORef Int)
progress [] = (,) <$> forkIO (return ()) <*> newIORef 0
progress xs = do
    r <- newIORef 0
    let go []     = return ()
        go (_:ys) = modifyIORef r (+1) >> go ys
    t <- forkIO (go xs)
    return (t, r)

stopRepl :: Repl -> IO ()
stopRepl = killThread . interpreter

newRepl :: IO Repl
newRepl = do
    inp <- newChan
    out <- newChan
    repl' defaultImports defaultExtensions inp out defaultWait defaultErrorWait defaultLineLength

defaultWait :: Maybe Double
defaultWait = Just 5

defaultErrorWait :: Maybe Int
defaultErrorWait = Just 3000

defaultLineLength :: Maybe Int
defaultLineLength = Just 512

defaultImports :: [String]
defaultImports
  = ["import Prelude hiding ((.), id)"
    ,"import GHC.TypeLits"
    ,"import qualified Data.Map as M"
    ,"import qualified Data.Foldable as F"
    ,"import qualified Data.Traversable as T"
    ,"import Control.Monad.Reader"
    ,"import Control.Monad.State"
    ,"import Control.Monad.Writer"
    ,"import Control.Monad.RWS"
    ,"import Control.Comonad"
    ,"import Control.Category"
    ,"import Data.Function hiding ((.), id)"
    ,"import Control.Arrow"
    ,"import Data.List"
    ,"import Data.Maybe"
    ,"import Data.Semigroup"
    ,"import Control.Monad"
    ,"import Control.Monad.Fix"
    ,"import Control.Applicative"
    ,"import Control.Lens"
    ,"import Data.Monoid"
    ,"import Data.Functor"
    ,"import Data.Typeable"]

defaultExtensions :: [ExtensionFlag]
defaultExtensions 
  = [Opt_DataKinds
    ,Opt_PolyKinds
    ,Opt_TypeFamilies
    ,Opt_TypeOperators
    ,Opt_DeriveFunctor
    ,Opt_DeriveDataTypeable
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

prompt_ :: Repl -> Input -> IO Output
prompt_ r i = do
    writeChan (inputChan r) i
    readChan (outputChan r)

-- | 'Repl' smart constructor.
repl' 
    :: [String]        -- ^ Imports, using normal Haskell import syntax
    -> [ExtensionFlag] -- ^ List of compiler extensions to use
    -> Chan Input      -- ^ Input channel
    -> Chan Output     -- ^ Output channel
    -> Maybe Double    -- ^ Maximum time to wait for a result, in seconds (default: 5)
    -> Maybe Int       -- ^ Maximum time to wait for an error, in microseconds (default: 3000)
    -> Maybe Int       -- ^ Maximum line length in 'Char' (default: 512)
    -> IO Repl
repl' imports exts inp out wait ewait len = do
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
                input' <- liftIO (readChan inp)
                liftIO . writeChan out =<< case input' of
                    Clear      -> do
                        setTargets []
                        void (load LoadAllTargets)
                        return $ Output []
                    Undefine _ -> do
                        tgs <- getTargets
                        liftIO (putStrLn $ sdoc tgs)
                        return $ Output []
                    Type s -> errors $ formatType <$> exprType s
                    Kind s -> errors $ formatType . snd <$> typeKind True s
                    Decl s -> errors $ do runDecls s; return $ Output ["OK."]
                    Bind s -> errors $ do void (runStmt ("let {" ++ s ++ "}") SingleStep); return $ Output ["OK."]
                    Expr s -> errors $ do
                        compiled <- dynCompileExpr $ "show (" ++ s ++ ")"
                        return $ Output [fromDyn compiled ""]
                    Info s -> errors $ do
                        names <- parseName s
                        infos <- concatMap (\(t,f,cs) -> sdoc t : sdoc f : map sdoc cs)
                               . catMaybes
                             <$> mapM getInfo names
                        return $ Output infos
    return $ Repl inp out interp wait ewait len
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

