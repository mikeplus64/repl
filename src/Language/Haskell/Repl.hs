module Language.Haskell.Repl 
    ( Repl
    -- * Making 'Repl's
    , newRepl
    , repl
    -- * Killing 'Repl's
    , stopRepl
    -- * Interaction
    , prompt
    , safePrompt
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

data Repl = Repl
    { input       :: Chan [String]
    , output      :: Chan [String]
    , interpreter :: ThreadId
    }

apply :: String -> String -> String
apply f x = f ++ " (" ++ x ++ ")"

enter :: Repl -> [String] -> String -> IO ()
enter r sts x = writeChan (input r) (sts ++ [x])

results :: Repl -> IO [String]
results = readChan . output

-- | Enter commands and an expression to a 'Repl', and immediately consume results.
-- Commands can be anything you can input in GHCi, other than imports.
prompt :: Repl 
       -> [String]    -- ^ Commands
       -> String      -- ^ Expression used for results
       -> IO [String] -- ^ _Lazy_ list of results
prompt r sts x = do
    enter r sts x
    results r

-- | Enter commands and an expression to a 'Repl', and immediately consume results.
-- However, truncate input to the given length, and stop the computation after the
-- given amount of time in seconds.
safePrompt :: Repl
           -> Double      -- ^ Time to wait (in seconds)
           -> Int         -- ^ Maximum result length in 'Char'
           -> [String]    -- ^ Commands
           -> String      -- ^ Expression used for results.
           -> IO [String]
safePrompt g d i sts x = do
    xs <- prompt g sts x
    t  <- newEmptyMVar
    w0 <- forkIO $ do
        threadDelay (floor (d*1000000))
        putMVar t ["Thread timed out."]
    w1 <- forkIO $ do
        (tr,ir)  <- prog xs
        threadDelay 3000
        killThread tr
        progress <- readIORef ir
        case take progress xs of
            hs@(_:_) -> ends (map (take i) hs) `seq` putMVar t hs
            _        -> putMVar t []
    r  <- takeMVar t
    killThread w0
    killThread w1
    return (map (take i) r)

-- | See if a lazy list has ended.
ends :: [a] -> Bool
ends []     = True
ends (_:xs) = ends xs

-- | See 'how far' a lazy list has evaluated.
prog :: [a] -> IO (ThreadId, IORef Int)
prog xs = do
    r <- newIORef 0
    let go []     = return ()
        go (_:ys) = modifyIORef r (+1) >> go ys
    t <- forkIO (go xs)
    return (t, r)

stopRepl :: Repl -> IO ()
stopRepl = killThread . interpreter

newRepl :: IO Repl
newRepl = join $ repl defaultImports defaultExtensions <$> newChan <*> newChan

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
repl :: [String]        -- ^ Imports, using normal Haskell import syntax.
     -> [ExtensionFlag] -- ^ List of compiler extensions to use
     -> Chan [String]   -- ^ Input channel
     -> Chan [String]   -- ^ Output channel
     -> IO Repl
repl imports exts inp out = do
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

    return $ Repl inp out interp
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

