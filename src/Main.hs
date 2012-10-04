-----------------------------------------------------------------------------
--
-- Module      :  Prolog
-- Copyright   :
-- License     :  GPL Nothing
--
-- Maintainer  :  Angelos Charalambidis <a.charalambidis@di.uoa.gr>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import Prolog

import WAM.Instruction
import WAM.Compile (wamCompileProg)
import WAM.Runtime (dumpCell, evalWam, wamExecute)
import WAM.Emit (wamEmitProg)

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Paths_wam
import Data.Version

import Control.Monad (when)
import Control.Monad.Trans (liftIO)

data Options = Options {
    inputFile :: String,
    outputFile :: Maybe String,
    wamFile :: Maybe String,
    onlyCompile :: Bool,
    verbose :: Bool
}

options :: [ OptDescr (Options -> IO Options) ]
options = [
    Option ['i'] ["input"] (ReqArg (\arg opt -> return opt{inputFile = arg})  "FILE")
        "input file",
    Option ['o'] ["output"] (ReqArg (\arg opt -> return opt{outputFile = Just arg}) "FILE")
        "output file",
    Option ['c'] ["compile"] (NoArg (\opt -> return opt{onlyCompile = True}))
        "just compile not run",
    Option ['w'] ["wam"] (ReqArg (\arg opt -> return opt{wamFile = Just arg}) "FILE")
        "wam file to run",
    Option ['V'] ["version"] (NoArg (\_ -> do
                                        prg <- getProgName
                                        hPutStrLn stderr (prg ++"-"++ showVersion version)
                                        exitWith ExitSuccess))
        "Show version",
    Option ['v'] ["verbose"] (NoArg (\opt -> return opt{verbose = True}))
        "Verbose mode",
    Option ['h'] ["help"] (NoArg (\_ -> do
                                     prg <- getProgName
                                     hPutStrLn stderr (usageInfo prg options)
                                     exitWith ExitSuccess))
        "Show help"
    ]
--            RegArg (\arg opt -> return opt



-- Main

main = do
    args <- getArgs

    let startOptions = Options { inputFile = undefined,
                                 outputFile = Nothing,
                                 wamFile = Nothing,
                                 onlyCompile = False,
                                 verbose = False
                               }

    let (actions,_,_) = getOpt Permute options args

    opts <- foldl (>>=) (return startOptions) actions

    let Options { inputFile = input, outputFile = output, onlyCompile = onlycompile } = opts

    h <- openFile input ReadMode
    fl <- hGetContents h

    compiled <- case parseprolog "error" fl of
                    Right x -> return $ wamCompileProg x
                    Left y -> print y >> exitWith (ExitFailure 1)

    case output of
        Nothing -> when (verbose opts) (putStr $ wamEmitProg compiled)
        Just out -> do
                ho <- openFile out WriteMode
                hPutStr ho $ wamEmitProg compiled
                hClose ho
    let (P gs _ _) = compiled

    when (onlycompile == False) $ do
        evalWam $ (wamExecute compiled) >>= \is -> mapM_ (\(v,i) -> dumpCell i >>= \st -> liftIO (putStrLn (v ++ "=" ++ st))) (zip gs is)
