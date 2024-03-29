{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-|
Module      : CliOptions
Description : CLI options parser in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

CLI (Command Line Interface) options parser in Javalette compiler.
-}
module CliOptions (
    -- * Operations
    compOptions
) where

import Data.Char
import Data.Foldable

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Common.Options

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

compOptions :: IO JlcOptions
compOptions = execParser cliParser

--------------------------------------------------------------------------------
-- Constructing the parser:
--------------------------------------------------------------------------------

allOpts :: Parser JlcOptions
allOpts =  JlcOptions
       <$> optInputs
       <*> optOutput
       <*> optOutFT
       <*> optCompilerFlags
       <*> optTCOnly
       <*> optAROnly
       <*> optPOOnly
       <*> optLRLevel
       <*> optOptLevel
       <*> optLLInputs
       <*> optLLIntermed

jlcHeader :: Doc
jlcHeader = text "The glorious Javalette Compiler."
       <$$> text "Copyright, 2016, Björn Tropf, Mazdak Farrokhzad"
       <$$> text "Distributed under GPL2 or any later version."

jlcProgDesc :: Doc
jlcProgDesc = hardline
           <> text "To compile and link a complete Javalette program,"
               </> text "run the compiler like so"
               </> parens (text "assuming it is on the PATH") <> colon
         <$$> enclose hardline hardline (indent 4 (text "jlc myprogram.jl"))
         <$$> text "which will create the file myprogram.out"
          </> parens (text "or myprogram.exe on Windows") <> dot
           <> hardline
         <$$> text "Jlc will determine how to handle files by their extensions."
          </> text "It will treat .ll files as llvm IR and .bc at llvm bitcode."
          </> text "All other files will be treated as .jl files."
           <> hardline
         <$$> text "For more advanced use cases, see options."

cliParser :: ParserInfo JlcOptions
cliParser = info (helper <*> allOpts) $
       fullDesc <> progDescDoc (Just jlcProgDesc) <> headerDoc (Just jlcHeader)

optOutput :: Parser (Maybe FilePath)
optOutput = optional $ strOption $
       long "output"
    <> short 'o'
    <> showDefaultWith (const "the default")
    <> metavar "FILE"
    <> help "FILE path to executable to produce"

parseOFT :: String -> ReadM OutFType
parseOFT pstr = case toLower <$> pstr of
    "exec" -> return OFTExec
    "asm"  -> return OFTAsm
    "bc"   -> return OFTBitcode
    x      -> readerError $ unwords ["Unrecognized output file type:", x ++ ",",
                                     "should be one of: exec | asm | bc"]

optOutFT :: Parser OutFType
optOutFT = option (str >>= parseOFT) $
       long "outtype"
    <> value OFTBitcode -- the default should be OFTEec, but for submission...
    <> metavar "exec | asm | bc"
    <> showDefaultWith (const "exec")
    <> help "exec for executable, asm for native assembly, bc for llvm bitcode"

optInputs :: Parser [FilePath]
optInputs = some $ strArgument $
       metavar "FILES..."
    <> help "FILE paths to javalette input files"

optTCOnly :: Parser Bool
optTCOnly = switch $
       long "typecheck"
    <> short 't'
    <> help "Only perform typechecking"

optAROnly :: Parser Bool
optAROnly = switch $
       long "alpha-rename"
    <> short 'a'
    <> help "Stop after alpha renaming"

optPOOnly :: Parser Bool
optPOOnly = switch $
       long "pre-opt"
    <> short 'p'
    <> help "Stop after pre-optimization"

onlyLLVM :: String -> Mod f a
onlyLLVM x = helpDoc $ Just $ text x </> text
    "Applies in conjunction with the llvm backend"

optLLInputs :: Parser [FilePath]
optLLInputs = many $ strOption $
       long "llvm-extras"
    <> short 'l'
    <> metavar "FILES..."
    <> onlyLLVM "FILE paths to additional llvm code for compilation"

optLLIntermed :: Parser Bool
optLLIntermed = switch $
       long "llvm-inter"
    <> short 'i'
    <> onlyLLVM "Emit all intermediary produced .ll files"

optCompilerFlags :: Parser CompilerFlags
optCompilerFlags = CompilerFlags <$> optWarnToError <*> optNoWarnUnused

optWarnToError :: Parser Bool
optWarnToError = switch $
       long "fwarn-error"
    <> short 'w'
    <> help "Turn all warnings into errors"

optNoWarnUnused :: Parser Bool
optNoWarnUnused = switch $
       long "fno-warn-unused"
    <> short 'n'
    <> help "Do not warn about unused parameter and variables"

optLRLevel :: Parser LRLevel
optLRLevel = optQuiet <|> optInfo <|> optWarn

optLR :: LRLevel -> Mod FlagFields LRLevel -> Parser LRLevel
optLR = flag LRWarn

optQuiet :: Parser LRLevel
optQuiet = optLR LRError $
       long "quiet"
    <> short 'q'
    <> help "Suppress all messages except for errors"

optWarn :: Parser LRLevel
optWarn = optLR LRWarn $
       long "warn"
    <> help "Suppress all messages except for errors and warnings (default)"

optInfo :: Parser LRLevel
optInfo = optLR LRInfo $
       long "info"
    <> help "Verbose mode. Show all types of messages including info messages"

optOptLevel :: Parser OptLevel
optOptLevel = asum $ zipWith3 opt gs [0..] ["0", "1", "2", "s", "z", "3", "4"]
    where gs = opt0 : repeat optn
          opt0 = const "No optimization (default)"
          optn = ("Optimization level " ++) . show
          opt g i llvm = flag Optimize0 (toEnum i) $
                        long ("O" ++ show i)
                     <> short (intToDigit i)
                     <> help (g i ++ ", for LLVM this is -O" ++ llvm)