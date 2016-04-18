module Grade where

import RunCommand
import KompTest

import Data.List
import Data.Maybe
import System.Directory hiding (makeAbsolute)
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.FilePath
--import System.Posix.Files
import Control.Exception
import Control.Monad
import System.Console.GetOpt

data Flag = SearchScript String |
            Extension String    |
            Back String
     deriving (Eq,Ord)

cmd :: String -> IO ()
cmd c = do
    --putStrLn c
    (out,err,code) <- runCommandStrWait c ""
    if err /= "" then do
        putStrLn out
        putStrLn err
    else
        return ()

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p = (</> p) <$> getCurrentDirectory

rmAll :: String -> IO ()
rmAll path = cmd $ "rm -r " ++ path

cpAll :: String -> IO ()
cpAll path = cmd $ "cp -R " ++ path 

unTar :: String -> String -> FilePath -> IO ()
unTar groupPath0 tarOpt subm =
    cmd $ unwords ["tar", "-C", groupPath0, concat ["-", tarOpt, "xvf"],
                   show (groupPath0 </> subm)]

make :: FilePath -> IO ()
make groupPath0 = cmd $ unwords ["make", "-C", show $ groupPath0 </> "src"]

maybeBuild :: String -> [(String, FilePath)] -> IO ()
maybeBuild _ []                           = return ()
maybeBuild groupPath0 ((tarOpt,subm) : _) = do
    unTar groupPath0 tarOpt subm
    make groupPath0

routeBackendTest :: [String] -> Maybe TestProg
routeBackendTest bs
    | null bs   = Nothing
    | otherwise = case bs of
        "JVM"  : _ -> Just testJVM
        "LLVM" : _ -> Just testLLVM
        "x86"  : _ -> Just testx86
        b      : _ -> error $ "Unknown backend: " ++ b

computeSubmissions :: [String] -> [(String, String)]
computeSubmissions allFiles =
    [(opts, s) | (opts, suff) <- [("z", ".tar.gz"),
                                  ("j", ".tar.bz2"),
                                  ("j", ".tar.bzip2"),
                                  ("", ".tar")],
                            s <- filter (suff `isSuffixOf`) allFiles]

testAll :: FilePath -> [String] -> [String] -> [FilePath] -> IO ()
testAll compiler bs exts [testSuitePath00, groupPath0] = do
    submissions <- computeSubmissions <$> getDirectoryContents groupPath0

    maybeBuild groupPath0 submissions

    let testSuitePath0 = groupPath0 </> "graderTestSuite"

    rmAll testSuitePath0

    cpAll $ testSuitePath00 </> "testsuite" ++ " " ++ testSuitePath0

    let exePath0 = groupPath0 </> compiler

    exePath <- makeAbsolute exePath0
    groupPath <- makeAbsolute groupPath0
    testSuitePath <- makeAbsolute testSuitePath0

    summary <- runInDirectory $ do
        let exeDir = takeDirectory exePath
        let libpath = groupPath </> "lib"
        let testProg = routeBackendTest bs
        let specs = testSpecs testProg exts libpath
        putStrLn $ "Running tests for " ++ exePath
        setCurrentDirectory exeDir
        forM specs (executeSpec testSuitePath exePath)

    putStrLn $ "Summary:\n" ++ unlines (map summaryLine summary)
    putStrLn $ "Credits total: " ++ show (sum [x | (_,x,_) <- summary])

    testsPassedCheck summary

executeSpec :: FilePath -> FilePath -> TestSpec -> IO (String, Int, [Bool])
executeSpec testSuitePath exePath (points, name, tests) = do
    putStrLn $ name ++ "..."
    results <- forM tests (executeTestCase testSuitePath exePath)
    putStrLn $ "Passed suites: " ++
                intercalate ", " [p | (p, rs) <- results, and rs]
    let tally = concatMap snd results
    return (name, if and tally then points else (0 :: Int), tally)

executeTestCase :: FilePath -> FilePath -> TestCase -> IO (FilePath, [Bool])
executeTestCase testSuitePath exePath (good, p, testFunction) = do
    testFiles <- getTestFilesForPath $ testSuitePath </> p
    putStrLn $ p ++ "..."
    rs <- testFunction exePath good testFiles
    report p rs
    return (p, rs)

checkPassed :: Foldable t2 => (t, t1, t2 Bool) -> Bool
checkPassed (name, points, tests) = and tests
-- testsPassed      tests = all (> 0) $ map (\(x, y, z) -> y) tests
testsPassedCheck :: Foldable t2 => [(t, t1, t2 Bool)] -> IO ()
testsPassedCheck tests = unless (head (map checkPassed tests)) $
                         putStrLn "Tests failed" >> exitWith (ExitFailure 1)

padl :: Int -> String -> String
padl n s = replicate (n - length s) ' ' ++ s

summaryLine :: Show a => (String, a, [Bool]) -> String
summaryLine (name, points, tests) =
    unwords [padl 2 (show points), name, "(",
             concat [show (length (filter id tests)),
                     "/", show (length tests) ++ ")"]]

type TestFunction = FilePath -> Bool -> [FilePath] -> IO [Bool]
type TestProg = String -> TestFunction
type TestCase = (Bool, String, TestFunction)
type TestSpec = (Int, String, [TestCase])

testSpecs :: Maybe TestProg -> [String] -> String -> [TestSpec]
testSpecs testProg exts libpath = testCompilationSpecs exts ++
                                  testBackendSpecs testProg exts libpath

testCoreSpecs :: TestSpec
testCoreSpecs =     (0, "Compiling core programs",
                        [(True, "good", testCompilation),
                         (False, "bad", testCompilation)])

testExtSpecs :: String -> TestSpec
testExtSpecs ext =  (0, "Compiling extension " ++ ext,
                       [(True, "extensions/"  ++ ext, testCompilation)])

testCompilationSpecs :: [String] -> [TestSpec]
testCompilationSpecs exts = testCoreSpecs : map testExtSpecs exts

testCoreBackendSpecs :: (a -> TestFunction) -> a -> TestSpec
testCoreBackendSpecs backend libpath =
                    (0, "Running core programs",
                        [(True, "good", backend libpath)])

testExtBackendSpecs :: (a -> TestFunction) -> a -> String -> TestSpec
testExtBackendSpecs backend libpath ext =
                    (1, "Running extension " ++ ext,
                        [(True, "extensions/" ++ ext, backend libpath)])

testBackendSpecs :: Maybe (a -> TestFunction) -> [String] -> a -> [TestSpec]
testBackendSpecs testProg exts libpath = maybe [] forBackend testProg
    where forBackend backend = testCoreBackendSpecs backend libpath :
                               map (testExtBackendSpecs backend libpath) exts

testBack :: Backend -> FilePath -> Bool -> [FilePath] -> IO [Bool]
testBack back cmd good fs = if good then test cmd fs back else return []

testJVM :: TestProg
testJVM = testBack . jvmBackend

testLLVM :: TestProg
testLLVM  = testBack . llvmBackend

testx86 :: TestProg
testx86 = testBack . x86Backend

flags :: [OptDescr Flag]
flags = [Option "s" ["search-compiler"]
            (ReqArg SearchScript "<compiler>") "search for the specified compiler",
         Option "x" ["extension"]
            (ReqArg Extension "<extension>") "specify extensions to test",
         Option "b" ["backend"]
            (ReqArg Back "<backend>") "specify backend"]

main :: IO ()
main = do
    argv <- getArgs
    case getOpt Permute flags argv of
        (opts,args,[]) -> do
                 let searchList0 = [s | SearchScript s <- opts]
                     compiler = if null searchList0 then "jlc" else head searchList0
                     exts = [e | Extension e <- opts]
                     bs = [b | Back b <- opts]
                 testAll compiler bs exts args
        (_,_,errs) -> do
                        hPutStrLn stderr (concat errs ++ usageInfo "" flags)
                        exitWith (ExitFailure 1)

 where defaultSearchList = []