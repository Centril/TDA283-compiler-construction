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
Module      : Common.FileOps
Description : Common file related operations in Javalette Compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Common file related operations in Javalette Compiler.
-}
module Common.FileOps where

import Control.Monad.Except

import System.Exit
import System.Process
import System.IO.Temp
import System.Directory

import Utils.Pointless

import Common.Computation

-- 'withSysTempDir': given a template, creates a temporary directory inside
-- the temporary directory of the system, e.g: /tmp/{unique(template)} and
-- passes that directory to the second argument which should produce an action.
withSysTempDir :: (MonadIO m, MonadError e m)
               => String -> (FilePath -> m a) -> m a
withSysTempDir template action = do
    sysTmp <- io getTemporaryDirectory
    tmpDir <- io $ createTempDirectory sysTmp template
    let rmRf = io $ removeDirectoryRecursive tmpDir
    action tmpDir `catchError` ((rmRf >>) . throwError) <* rmRf

-- | 'execInOut': simplified version of 'execProcess' which provides input files
-- and one output file.
execInOut :: (String -> IOComp s String)
          -> FilePath -> [String] -> [FilePath] -> FilePath -> IOComp s ()
execInOut onErr cmd args ins out =
    void $ execProcess onErr cmd (ins ++ ["-o", out] ++ args) ""

-- | 'execProcess': executes a process in second argument given the arguments,
-- stdin and a onErr handler. During normal execution, the function will yield
-- the standard output of the called process as a String.
execProcess :: (String -> IOComp s String)
            -> FilePath -> [String] -> String -> IOComp s String
execProcess onErr cmd args stdin = do
    (code, stdout, stderr) <- io $ readProcessWithExitCode cmd args stdin
    if code /= ExitSuccess then onErr stderr else return stdout

-- | 'writeF': lifted 'writeFile' operation.
writeF :: MonadIO m => FilePath -> String -> m ()
writeF = io .| writeFile

-- | 'readF': lifted 'readFile' operation.
readF :: MonadIO m => FilePath -> m String
readF = io . readFile

-- | 'readF': lifted 'copyFile' operation.
copyF :: MonadIO m => FilePath -> FilePath -> m ()
copyF = io .| copyFile