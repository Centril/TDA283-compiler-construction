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
Module      : Common.Options
Description : Options and configurations in Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Options and configurations in Javalette compiler.
-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Options where

import Data.List (partition, nub)
import Data.Map (Map, fromList)
import Data.Maybe

import Control.Arrow

import Control.Lens hiding ((<.>))

import System.Info
import System.FilePath

--------------------------------------------------------------------------------
-- Data Types:
--------------------------------------------------------------------------------

-- | 'JlcOptions': all options Jlc can handle.
data JlcOptions = JlcOptions {
      _inputFiles    :: [FilePath]     -- ^ .jl files to compile
    , _outputFile    :: Maybe FilePath -- ^ path to executable to produce
    , _outFileType   :: OutFType       -- ^ type of output file
    , _compileFlags  :: CompilerFlags  -- ^ compiler flags
    , _typecheckOnly :: Bool           -- ^ only perform typechecking?
    , _logLevel      :: LRLevel        -- ^ Log reporting level
    , _optLevel      :: OptLevel       -- ^ optimization levels
    , _llInputFiles  :: [FilePath]     -- ^ .ll files to assemble (extra)
    , _llIntermed    :: Bool           -- ^ output .ll intermediaries?
    } deriving (Eq, Ord, Show, Read)

-- | 'CompilerFlags': flags for the Javalette compiler.
data CompilerFlags = CompilerFlags {
      _warnToError  :: Bool          -- ^ convert all warnings to errors?
    , _noWarnUnused :: Bool          -- ^ warn about unused params/variables?
    } deriving (Eq, Ord, Show, Read)

-- | 'OptLevel': Optimization levels, semantics depends on backend.
data OptLevel = Optimize0 -- ^ No optimizations for all backends.
              | Optimize1 -- ^ -O1 for LLVM
              | Optimize2 -- ^ -O2 for LLVM
              | Optimize3 -- ^ -Os for LLVM
              | Optimize4 -- ^ -Oz for LLVM
              | Optimize5 -- ^ -O3 for LLVM
              | Optimize6 -- ^ -O4 for LLVM
    deriving (Eq, Ord, Enum, Show, Read)

-- | 'LRLevel': Logging Reporting Level in increasing levels of verbosity.
data LRLevel = LRError | LRWarn | LRInfo
    deriving (Eq, Ord, Enum, Show, Read)

-- | 'OutFType': available output file types of compiler.
data OutFType = OFTExec | OFTAsm | OFTBitcode
    deriving (Eq, Ord, Enum, Show, Read)

-- | 'InputFType': classifications of input file types.
data InputFType = IFJavalette | IFLlvm | IFLlvmBc
    deriving (Eq, Ord, Enum, Show, Read)

-- | 'IFMap': classified lists of filepaths.
type IFMap = Map InputFType [FilePath]

--------------------------------------------------------------------------------
-- Lenses and Prisms:
--------------------------------------------------------------------------------

makeLenses ''JlcOptions
makeLenses ''CompilerFlags

makePrisms ''OptLevel
makePrisms ''LRLevel
makePrisms ''OutFType
makePrisms ''InputFType

--------------------------------------------------------------------------------
-- Operations:
--------------------------------------------------------------------------------

-- | 'classifyInputs': from options to classified lists of filepaths.
classifyInputs :: JlcOptions -> IFMap
classifyInputs opts = fromList $ second nub <$>
    [(IFJavalette, jls         ),
     (IFLlvm,      lls1 ++ lls2),
     (IFLlvmBc,    bcs         )]
    where (lls1, (lls2, (bcs, jls))) =
            _llInputFiles &&&
            second (withExt "bc") . withExt "ll" . _inputFiles $ opts

-- | 'determineOutput': from options to the end product to write to as a
-- filepath. This assumes that an end product will be produced.
determineOutput :: JlcOptions -> FilePath
determineOutput opts = fromMaybe ifhead $ _outputFile opts
    where (ifhead, oft) = (-<.> re) . head . _inputFiles &&& _outFileType $ opts
          re = case oft of
               OFTExec    -> if isWindows then "exe" else "out"
               OFTAsm     -> "s"
               OFTBitcode -> "bc"

-- | 'withExt': given an extension, splits list of filepaths into one with the
-- given extension and one without.
withExt :: String -> [FilePath] -> ([FilePath], [FilePath])
withExt = partition . hasExt

-- | 'hasExt': checks if the given extension is part of a filepath.
-- The extension is specified without the point at the start.
hasExt :: String -> FilePath -> Bool
hasExt ext fp = '.' : ext == takeExtension fp

-- | 'isWindows': are we on a Windows machine?
isWindows :: Bool
isWindows = os == "mingw32"