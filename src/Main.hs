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
import Prolog.Parser

import WAM.Instruction
import WAM.Compile (wamCompileProg, wamCompileGoal)
import WAM.Runtime (evalWam, wamExecute)
import WAM.Runtime.Trace (dumpCell, runTraceT, traceCommand)
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
    verbose :: Bool,
    trace :: Bool
}

options :: [ OptDescr (Options -> IO Options) ]
options = [
    Option ['i'] ["input"]   (ReqArg (\arg opt -> return opt{inputFile = arg})  "FILE")
        "input file",
    Option ['o'] ["output"]  (ReqArg (\arg opt -> return opt{outputFile = Just arg}) "FILE")
        "output file",
    Option ['c'] ["compile"] (NoArg (\opt -> return opt{onlyCompile = True}))
        "just compile not run",
    Option ['t'] ["trace"]   (NoArg (\opt -> return opt{trace = True}))
        "show trace",
    Option ['w'] ["wam"]     (ReqArg (\arg opt -> return opt{wamFile = Just arg}) "FILE")
        "wam file to run",
    Option ['V'] ["version"] (NoArg (\_ -> do
                                        prg <- getProgName
                                        hPutStrLn stderr (prg ++"-"++ showVersion version)
                                        exitWith ExitSuccess))
        "Show version",
    Option ['v'] ["verbose"] (NoArg (\opt -> return opt{verbose = True}))
        "Verbose mode",
    Option ['h'] ["help"]    (NoArg (\_ -> do
                                             prg <- getProgName
                                             hPutStrLn stderr (usageInfo prg options)
                                             exitWith ExitSuccess))
        "Show help"
    ]


-- Main

main = do
    args <- getArgs

    let startOptions = Options { inputFile = undefined,
                                 outputFile = Nothing,
                                 wamFile = Nothing,
                                 onlyCompile = False,
                                 verbose = False,
                                 trace = False
                               }

    let (actions,_,_) = getOpt Permute options args

    opts <- foldl (>>=) (return startOptions) actions

    let Options { inputFile = input
                , outputFile = output
                , onlyCompile = onlycompile
                } = opts

    compiled <- compilePrologFromFile input

    case output of
        Nothing -> when (verbose opts) (outputWam compiled output)
        Just _  -> outputWam compiled output

    when (onlycompile == False) $
        runWam (trace opts) compiled


compilePrologFromFile filename =
    withFile filename ReadMode $ \hdl -> do
        content <- hGetContents hdl
        compileProlog content

compileProlog prog = 
    case parseprolog "error" prog of
        Right (g, p) ->
            return $ (wamCompileGoal g 0, wamCompileProg p)
        Left y -> do
            print y
            exitWith (ExitFailure 1)

outputWam prog out = 
    let writeProg ho prog = do 
            hPutStr ho $ wamEmitProg prog
            hClose ho
        openHandle (Just filename) = openFile filename WriteMode
        openHandle Nothing         = return stdout
    in do 
        ho <- openHandle out
        writeProg ho prog

outputWamVars gs = 
    let printBinding (v,i) = do
            cell <- dumpCell i
            liftIO $ putStr (v ++ "=" ++ cell ++ "\n")
    in mapM_ printBinding gs

runWam trace (compiledGoal, compiledProg) = 
    let run = evalWam . runTrace 
        runTrace  = 
            if trace 
            then runTraceT traceCommand 
            else runTraceT notrace

        notrace a = return ()
    in run $ do 
        gs <- wamExecute compiledProg compiledGoal
        outputWamVars gs

