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
module CliOptions where

import Data.Char
import Data.Foldable

import System.Environment

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Utils.Pointless

import Common.Options

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

-- | 'parseCLI': parses command line interface options given a list of "words".
parseCLI :: [String] -> IO JlcOptions
parseCLI = execParse cliParser

--------------------------------------------------------------------------------
-- Parsing:
--------------------------------------------------------------------------------

-- | 'execParse': executes an option parser given list of "words" to parse.
execParse :: ParserInfo a -> [String] -> IO a
execParse = handleParseResult .| execParserPure defaultPrefs

--------------------------------------------------------------------------------
-- Constructing the parser:
--------------------------------------------------------------------------------

allOpts :: Parser JlcOptions
allOpts =  JlcOptions
       <$> optInputs
       <*> optOutput
       <*> optCompilerFlags
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
         <$$> text "which will create the file myprogram"
          </> parens (text "or myprogram.exe on Windows") <> dot
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
    <> help "FILE path to executuable to produce"

optInputs :: Parser [FilePath]
optInputs = some $ strArgument $
       metavar "FILES..."
    <> help "FILE paths to javalette input files"

onlyLLVM :: String -> Mod f a
onlyLLVM x = helpDoc $ Just $ text x </> text
    "This only applies when used in conjunction with the llvm backend."

optLLInputs :: Parser [FilePath]
optLLInputs = many $ strOption $
       long "llvm-extras"
    <> short 'l'
    <> metavar "FILES..."
    <> onlyLLVM "FILE paths to additional llvm code to compile and link with."

optLLIntermed :: Parser Bool
optLLIntermed = switch $
       long "llvm-inter"
    <> short 'i'
    <> onlyLLVM "Enable emitting intermediary produced .ll files."

optCompilerFlags :: Parser CompilerFlags
optCompilerFlags = CompilerFlags <$> optWarnToError <*> optNoWarnUnused

optWarnToError :: Parser Bool
optWarnToError = switch $
       long "fwarn-error"
    <> short 'w'
    <> help "Make all warnings into errors."

optNoWarnUnused :: Parser Bool
optNoWarnUnused = switch $
       long "fno-warn-unused"
    <> short 'n'
    <> help "Do not warn about unused parameter and variables."

optLRLevel :: Parser LRLevel
optLRLevel = optQuiet <|> optInfo <|> optWarn

optLR :: LRLevel -> Mod FlagFields LRLevel -> Parser LRLevel
optLR = flag LRWarn

optQuiet :: Parser LRLevel
optQuiet = optLR LRError $
       long "quiet"
    <> short 'q'
    <> help "Supress all messages except for errors."

optWarn :: Parser LRLevel
optWarn = optLR LRWarn $
       long "warn"
    <> help "Supress all messages except for errors and warnings (default)."

optInfo :: Parser LRLevel
optInfo = optLR LRInfo $
       long "info"
    <> help "Verbose mode. Show all types of messages including info messages."

optOptLevel :: Parser OptLevel
optOptLevel = asum $ zipWith3 opt gs [0..] ["0", "1", "2", "s", "z", "3", "4"]
    where gs = opt0 : repeat optn
          opt0 = const "No optimization (default)"
          optn = ("Optimization level " ++) . show
          opt g i llvm = flag Optimize0 (toEnum i) $
                        long ("O" ++ show i)
                     <> short (intToDigit i)
                     <> help (g i ++ ", for LLVM this is -O" ++ llvm ++ ".")