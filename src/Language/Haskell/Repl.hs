module Language.Haskell.Repl 
    ( Repl(patienceForResult,patienceForErrors,lineLength)
    -- * Making 'Repl's
    , newRepl
    , repl
    -- * Killing 'Repl's
    , stopRepl
    -- * Interaction
    , prompt
    , enter
    , results
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Dynamic
import GHC
import GHC.Paths (libdir)
import DynFlags
import GhcMonad
import Outputable (showSDocForUser, ppr, neverQualify)
import Data.IORef
import Data.Maybe
import Data.List (union)
import Data.Foldable (for_)

data Repl = Repl
    { input             :: Chan [String]
    , output            :: Chan [String]
    , interpreter       :: ThreadId
    , patienceForResult :: Maybe Double
    , patienceForErrors :: Maybe Int
    , lineLength        :: Maybe Int
    }

apply :: String -> String -> String
apply f x = f ++ " (" ++ x ++ ")"

enter :: Repl 
      -> [String] -- ^ Commands
      -> String   -- ^ Expression used for results.
      -> IO ()
enter r sts x = writeChan (input r) (sts ++ [x])

-- | Naiively get the next set of results. This _does not_ take into account
-- 'patienceForResults', 'patienceForErrors', or 'lineLength'.
results :: Repl -> IO [String]
results = readChan . output

-- | Enter commands and an expression to a 'Repl', and immediately consume results.
-- However, truncate input to the given length, and stop the computation after the
-- given amount of time in seconds.
prompt 
    :: Repl
    -> [String]    -- ^ Commands
    -> String      -- ^ Expression used for results.
    -> IO [String]
prompt r xs x = do
    let trimLines = case lineLength r of
            Just l -> map (take l)
            _      -> id
    enter r xs x
    lazyResults <- results r
    final       <- newEmptyMVar
    timeout     <- forkIO $ patienceForResult r `for_` \ p -> do
        threadDelay (floor (p*1000000))
        putMVar final ["Thread timed out."]
    attempt <- forkIO $ case patienceForErrors r of
        Just p -> do
            (tr,ir)  <- progress lazyResults
            threadDelay p
            killThread tr
            elems <- readIORef ir
            let hs = take elems xs
            ends (trimLines hs) `seq` putMVar final hs
        _ -> putMVar final (trimLines lazyResults)

    fin <- takeMVar final
    killThread timeout
    killThread attempt
    return fin

-- | See if a lazy list has ended.
ends :: [a] -> Bool
ends []     = True
ends (_:xs) = ends xs

-- | See 'how far' a lazy list has evaluated.
progress :: [a] -> IO (ThreadId, IORef Int)
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
    repl defaultImports defaultExtensions inp out defaultWait defaultErrorWait defaultLineLength

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
    ,"import Data.Semigroup"
    ,"import Control.Monad"
    ,"import Control.Monad.Fix"
    ,"import Control.Applicative"
    ,"import Control.Lens"
    ,"import Data.Monoid"
    ,"import Data.Functor"
    ,"import Data.Typeable"]

defaultExtensions :: [ExtensionFlag]
defaultExtensions = glasgowExtsFlags `union` [ Opt_DataKinds, Opt_PolyKinds, Opt_TypeFamilies, Opt_TypeOperators, Opt_GADTs ]

-- | 'Repl' smart constructor.
repl :: [String]        -- ^ Imports, using normal Haskell import syntax
     -> [ExtensionFlag] -- ^ List of compiler extensions to use
     -> Chan [String]   -- ^ Input channel
     -> Chan [String]   -- ^ Output channel
     -> Maybe Double    -- ^ Maximum time to wait for a result, in seconds (default: 5)
     -> Maybe Int       -- ^ Maximum time to wait for an error, in microseconds (default: 3000)
     -> Maybe Int       -- ^ Maximum line length in 'Char' (default: 512)
     -> IO Repl
repl imports exts inp out wait ewait len = do
    interp <- forkIO $
        runGhc (Just libdir) $ do
            dflags <- session
            forever $ do
                import_ imports
                x   <- liftIO (readChan inp)
                msg <- liftIO (newIORef [])

                forM_ (init x) (\s ->
                    runDeclOrStmt s 
                  `gcatch` 
                    \e@SomeException{} -> liftIO (modifyIORef msg (++ lines (show e))))

                liftIO . modifyIORef msg . flip (++) =<<
                    case last x of
                        ':':c:' ':arg -> case c of
                            't' -> errs (showSDocForUser dflags neverQualify . ppr) (snd . splitForAllTys <$> exprType arg)
                            'k' -> errs (showSDocForUser dflags neverQualify . ppr) (snd <$> typeKind True arg)
                            'd' | arg == "all" -> do
                                setTargets []
                                _<-load LoadAllTargets
                                return ["Cleared memory."]
                                
                            _   -> return ["Invalid command."]
                        e | isDecl e  -> errs
                                (const "OK.")
                                (runDecls e)
                          | otherwise -> case words e of
                                ("let":_) -> errs 
                                    (const "OK.")
                                    (runStmt e SingleStep)
                                _ -> errs
                                    (`fromDyn` "")
                                    (dynCompileExpr ("show" `apply` e))

                liftIO $ writeChan out =<< readIORef msg

    return $ Repl inp out interp wait ewait len 
  where
    runDeclOrStmt s 
        | isDecl s  = void (runDecls s)
        | otherwise = void (runStmt ("let {" ++ s ++ "}") SingleStep)

    isDecl x = maybe False decl (listToMaybe (words x))
    decl x = case x of
        "instance" -> True
        "data"     -> True
        "class"    -> True
        "type"     -> True
        _          -> False


    errs :: (a -> String) -> Ghc a -> Ghc [String]
    errs f x = fmap (lines . f) x 
      `gcatch` \e@SomeException{} 
               -> return 
                    ( -- map (dropWhile isSpace . dropWhileEnd isSpace)
                      lines 
                    . show 
                    $ e)

    import_ = mapM (fmap IIDecl . parseImportDecl) >=> setContext
    getExts = foldr (fmap . flip xopt_set) id
    session = do
        s <- getProgramDynFlags
        _ <- setSessionDynFlags (getExts exts s)
        getSessionDynFlags

