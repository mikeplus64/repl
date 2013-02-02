{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Repl 
    ( Repl
    -- * Construction
    , newRepl
    , repl'
    , defaultExtensions
    , defaultImports
    , defaultLineLength
    , defaultPatience
    -- * Stopping
    , stopRepl
    -- * Interaction
    , Input(..)
    , ReplOutput(..)
    , Output(..) 
    , prompt
    , prompt_
    , send
    , result
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
import Data.List
import Text.Parsec hiding (many,(<|>),newline)
import Text.Parsec.String
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Extension as H

import GHC
import GHC.Paths
import DynFlags
import GhcMonad
import Outputable (showSDocForUser, Outputable, ppr, neverQualify)
import HscTypes
import OccName

data Repl a = Repl
    { inputChan         :: Chan Input
    , outputChan        :: Chan (ReplOutput a)
    , interpreter       :: ThreadId
    , processOutput     :: Dynamic -> IO a
    , buildExpr         :: String  -> String
    , patience          :: Double
    , lineLength        :: Int
    }

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

data ReplOutput a
    = ReplError String
    | GhcError  String
    | Output    [String]
    | Result    a-- [String]
  deriving Show

data Output
    = OK        [String]
    | Exception [String] String
    | Errors    [String]
    | Partial   [String]
    | Timeout   [String]
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

parseMode :: H.ParseMode
parseMode = H.defaultParseMode 
    { H.extensions = H.knownExtensions \\
        [ H.TemplateHaskell
        , H.CPP
        , H.ForeignFunctionInterface
        , H.UnliftedFFITypes
        , H.XmlSyntax
        , H.MagicHash
        , H.HereDocuments
        , H.QuasiQuotes
        , H.NPlusKPatterns
        , H.UnboxedTuples ] 
    }

parseType, parseKind, parseInfo, parseDecl, parseStmt, parseExpr, parseUndefine, parseClear, parseInput :: Parser Input
parseType = simpl 't' Type
parseKind = simpl 'k' Kind
parseInfo = simpl 'i' Info
parseDecl = do
    decl <- getInput
    guard (valid (H.parseDeclWithMode parseMode) decl)
    return (Decl decl)
parseStmt = do
    stmt <- getInput
    case H.parseStmtWithMode parseMode stmt of
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
prettyOutput (Partial s)     = s
prettyOutput (Errors errs)   = errs
prettyOutput (Exception s e) = overLast (++ ("*** Exception: " ++ e)) s
prettyOutput (Timeout [])    = ["*** Timed out"]
prettyOutput (Timeout s)     = overLast (++ "*** Timed out") s

-- | Send input.
send :: Repl a -> Input -> IO ()
send = writeChan . inputChan

-- | Naiively get the next set of results. This /does not/ take into account
-- 'patiences', 'patienceForErrors', or 'lineLength'. However, due
-- to laziness, this may not matter.
result :: Repl a -> IO (ReplOutput a)
result = readChan . outputChan

{-# INLINE index #-}
index :: Int -> [a] -> Maybe a
i `index` ys
    | i >= 0    = go 0 ys
    | otherwise = Nothing
  where
    go _ [] = Nothing
    go j (x:xs) 
        | j == i    = Just x
        | otherwise = go (j+1) xs

{-# INLINE overHead #-}
overHead :: (a -> a) -> [a] -> [a]
overHead f xs' = case xs' of
    x:xs -> f x : xs
    _    -> []
 
{-# INLINE overLast #-}
overLast :: (a -> a) -> [a] -> [a]
overLast f = go
  where
    go []     = []
    go [x]    = [f x]
    go (x:xs) = x : go xs

{-# INLINE lengthAt #-}
lengthAt :: Int -> [[a]] -> Int
lengthAt i = maybe 0 length . index i

-- | Same as 'prompt_', except it parses the input, and pretty prints the results.
prompt
    :: Repl [String]
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
    :: Repl [String]
    -> Input
    -> IO Output
prompt_ repl x = do
    send repl x
    results' <- result repl

    unlessRedundant results' $ \ results -> do
        -- outputs is used iff an exception is raised by the compiled input.
        -- This was a DList, but I didn't find any real advantage of it over
        -- [String] -- snoc was cheap but toList very expensive.
        outputs :: IORef [String]   <- newIORef []
        threads :: IORef [ThreadId] <- newIORef []
        final   :: MVar Output      <- newEmptyMVar
        let push    c   = do
                output <- readIORef outputs
                if lengthAt (length output - 1) output > lineLength repl
                  then putMVar final (Partial output)
                  else writeIORef outputs (overHead (c:) output)
            newline     = modifyIORef outputs ([]:)
            readOutput  = reverse . map reverse <$> readIORef outputs
            fork f      = do
                thread <- forkIO $ f `catch` \e@SomeException{} -> do
                    output <- readOutput
                    putMVar final (Exception output (show e))
                modifyIORef threads (thread:)

        -- Time out
        -- This can return only Timeout <what was consumed so far>
        fork $ do
            threadDelay (floor (patience repl * 1000000))
            output <- readOutput
            putMVar final (Timeout output)

        -- Read characters off of results, and "push" them to outputs.
        fork $ do
            forM_ results $ \l -> do
                newline
                forM_ l push
            putMVar final . OK =<< readIORef outputs
        
        output <- takeMVar final
        mapM_ killThread =<< readIORef threads
        return output
  where
    trim = take (lineLength repl)

    -- | Don't bother with things other than an actual result from an expression -- they will be loaded "instantly"
    unlessRedundant (ReplError s) _ = return . Errors . map trim . lines $ s
    unlessRedundant (GhcError  s) _ = return . Errors . map trim . lines $ s
    unlessRedundant (Output s) _    = return . OK . map trim $ s
    unlessRedundant (Result s) f    = f s

stopRepl :: Repl a -> IO ()
stopRepl = killThread . interpreter

-- | Default GHCi-like 'Repl'
newRepl :: IO (Repl [String])
newRepl = do
    inp <- newChan
    out <- newChan
    repl' inp out 
        defaultImports 
        defaultExtensions 
        defaultBuildExpr 
        defaultProcessOutput 
        defaultPatience
        defaultLineLength

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
    ,"import Data.Bits"
    ,"import Data.Array"
    ,"import Data.Ix"
    ,"import Data.Functor"
    ,"import Data.Typeable"
    ,"import Data.Monoid"
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
    ,Opt_StandaloneDeriving
    ,Opt_MultiParamTypeClasses
    ,Opt_GADTs]

-- | defaultLineLength = 512
defaultLineLength :: Int
defaultLineLength = 512

-- | defaultPatience = 5
defaultPatience :: Double
defaultPatience = 5

defaultBuildExpr :: String -> String
defaultBuildExpr x = "show (" ++ x ++ ")"

defaultProcessOutput :: Dynamic -> IO [String]
defaultProcessOutput d = return (lines (fromDyn d ""))

-- | 'Repl' smart constructor.
repl' 
    :: Chan Input           -- ^ Input channel
    -> Chan (ReplOutput a)  -- ^ Output channel
    -> [String]             -- ^ Imports, using normal Haskell import syntax
    -> [ExtensionFlag]      -- ^ List of compiler extensions to use
    -> (String -> String)   -- ^ Used to build the expression actually sent to GHC
    -> (Dynamic -> IO a)    -- ^ Used to send output to the output 'Chan'.
    -> Double               -- ^ Maximum time to wait for a result, in seconds
    -> Int                  -- ^ Maximum line length in 'Char'
    -> IO (Repl a)
repl' inp out imports exts build process wait len = do
    interp <- forkIO $
        runGhc (Just libdir) $ do
            dflags <- mkSession
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
                i'   <- liftIO (readChan inp)
                liftIO . writeChan out =<< case i' of
                    Clear      -> do
                        setTargets []
                        void (load LoadAllTargets)
                        return (Output ["Cleared memory."])

                    Undefine s' -> fmap Output $ 
                        forM (words s') $ \s -> do
                            let eqs :: NamedThing a => a -> Bool
                                eqs n = occNameString (getOccName n) == s
                            session <- getSession
                            setSession session
                                { hsc_IC = (hsc_IC session)
                                    { ic_tythings = filter (not . eqs) (ic_tythings (hsc_IC session)) }
                                }
                            return "OK"

                    Type s -> errors $ formatType <$> exprType s
                    Kind s -> errors $ formatType . snd <$> typeKind True s
                    Decl s -> errors $ do _names <- runDecls s; return (Output ["OK."]) --  ["OK."]
                    Stmt s -> errors $ do void (runStmt s SingleStep); return (Output ["OK."])
                    Expr s -> errors $ do
                        compiled <- dynCompileExpr (build s)
                        built    <- liftIO (process compiled)
                        return (Result built)
                    Info s -> errors $ do
                        names <- parseName s
                        infos <- concatMap (\(t,f,cs) -> sdoc t : sdoc f : map sdoc cs)
                               . catMaybes
                             <$> mapM getInfo names
                        return $ Output infos
    return Repl 
        { inputChan      = inp
        , outputChan     = out
        , interpreter    = interp
        , processOutput  = process
        , buildExpr      = build
        , patience       = wait
        , lineLength     = len
        }
  where
    errors x = x `gcatch` \ e@SomeException{} -> 
        case fromException e :: Maybe ErrorCall of
            Just _ -> return $ ReplError (show e)
            _      -> return $ GhcError  (show e)

    import_ = mapM (fmap IIDecl . parseImportDecl) >=> setContext
    getExts = foldr (fmap . flip xopt_set) id
    mkSession = do
        s <- getProgramDynFlags
        _ <- setSessionDynFlags 
            $ (\d -> d { safeHaskell = Sf_Safe })
            . flip dopt_set Opt_DoCoreLinting 
            $ getExts exts s

        getSessionDynFlags

