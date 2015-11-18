{-
	Copyright (C) 2011-2013 Dr. Alistair Ward

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Defines a performance-test, based on randomly generated /virtual/ files.
-}

module Squeeze.Test.Performance(
-- * Functions
	run
) where

import qualified	Control.Monad
import qualified	Control.Monad.Writer
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Factory.Math.Probability
import qualified	Squeeze.Data.CommandOptions	as Data.CommandOptions
import qualified	Squeeze.Data.File		as Data.File
import qualified	Squeeze.Data.FileCombination	as Data.FileCombination
import qualified	Squeeze.Squeeze			as Squeeze
import qualified	System.IO
import qualified	System.Random

{- |
	* Find the best-fit, into the available space, using a set composed from the specified number of virtual files.

	* The files have a random size, conforming to the specified distribution.

	* The generated file-name, is merely a counter.

	* CAVEAT: fewer files than requested may be used, because depending on the requested distribution, some may overflow the maximum permissible size.
-}
run :: (Factory.Math.Probability.Distribution distribution, RealFrac ratio)
	=> Data.CommandOptions.CommandOptions ratio
	-> Int	-- ^ The number of virtual files to randomly generate.
	-> distribution
	-> IO [Data.FileCombination.FileCombination]
run commandOptions fileCount probabilityDistribution	= do
	randomGen	<- Data.Maybe.maybe System.Random.getStdGen {-use the global random-number generator-} (
		return {-to IO-monad-} . System.Random.mkStdGen	-- Seed the random-number generator as specified.
	 ) $ Data.CommandOptions.getMaybeRandomSeed commandOptions	-- Select a random-number generator.

	let
		(acceptedFileSizeAndPathList, logFile)	= Control.Monad.Writer.runWriter . Data.File.selectSuitableFileSizes (
			<= Data.CommandOptions.getMaximumBytes commandOptions	-- This may reduce the requested number of files, but compensating distorts the requested distribution.
		 ) . (
			`zip` map show [0 :: Int ..]	-- Construct a Data.File.FileSizeAndPath, by using a counter as the file-name.
		 ) . take fileCount . filter (
			>= 0				-- A suitable distribution shouldn't generate negative file-sizes.
		 ) . map (
			ceiling :: Double -> Integer	-- File-sizes are integral.
		 ) $ Factory.Math.Probability.generatePopulation probabilityDistribution randomGen

	Control.Monad.when (Data.CommandOptions.getVerbosity commandOptions > minBound) . System.IO.hPutStrLn System.IO.stderr $ Data.List.intercalate "\n" logFile

	Squeeze.distributeAndFindBestFit commandOptions acceptedFileSizeAndPathList

