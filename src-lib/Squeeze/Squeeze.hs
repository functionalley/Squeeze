{-
	Copyright (C) 2010-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]

	* Returns combinations of the specified files, which fit into the available space, without wasting more than the specified ratio.

	* Any directory-names are treated as atomic units, rather than individual files.

	* Because of the exponential growth of possible combinations,
	an /exact/ match for the available space is frequently found with a surprisingly small set of files.
-}

module Squeeze.Squeeze(
-- * Functions
	findCombinations,
	findBestFit,
	distributeAndFindBestFit,
	partitionEmptyFilesAndDistributeAndFindBestFit
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Control.Concurrent
import qualified	Control.Monad
import qualified	Data.List
import qualified	Factory.Data.Interval
import qualified	Squeeze.Control.Concurrent.DivideAndConquer	as Control.Concurrent.DivideAndConquer
import qualified	Squeeze.Data.CommandOptions			as Data.CommandOptions
import qualified	Squeeze.Data.File				as Data.File
import qualified	Squeeze.Data.FileCombination			as Data.FileCombination
import qualified	System.IO

{- |
	* Checks that the total aggregate 'Data.File.FileSize', meets or exceeds 'minimumBytes'.

	* Drops excessively large files, assuming that the file-list has been sorted by size, largest first.

	* Generates up to @2^n@ combinations of the @n@ specified files; the algorithm is similar to 'Data.List.subsequences', except that unproductive lines are immediately terminated.
	This is the performance bottle-neck, and though there may be simpler and faster algorithms, the key attribute is that it operates in constant space.

	* CAVEAT: assumes files have been sorted by decreasing size.

	* The algorithm is stable, in that it maintains the specified file-order within each combination;
	though the order in which the combinations are concatenated is rather arbitrary.
-}
findCombinations
	:: Factory.Data.Interval.Interval Data.File.FileSize	-- ^ The closed interval of acceptible aggregate size, for file-combinations.
	-> [Data.File.FileSizeAndPath]				-- ^ The list of file-names & sizes, ordered by decreasing size.
	-> [Data.FileCombination.FileCombination]		-- ^ The resulting unordered list of suitable file-combinations.
findCombinations (minimumCombinationSize, maximumCombinationSize)	= filter (
	Data.FileCombination.hasSizeBy (>= minimumCombinationSize)
 ) . (
	Data.FileCombination.nullFileCombination :
 ) . nonEmptyCombinations minimumCombinationSize . uncurry zip . (
	id &&& Data.File.accumulateSize			-- Associate the list of possible files with its accumulating size.
 ) . dropWhile (
	Data.File.hasSizeBy (> maximumCombinationSize)	-- Remove files which individually exceed the maximum permissible; assuming they've been sorted by decreasing size.
 ) where
	nonEmptyCombinations :: Data.File.FileSize -> [(Data.File.FileSizeAndPath, Data.File.FileSize)] -> [Data.FileCombination.FileCombination]
	nonEmptyCombinations _ []	= []
	nonEmptyCombinations minimumBytes ((fileSizeAndPath, aggregateSize) : remainder)
		| aggregateSize < minimumBytes	= []	-- Even if all the files are selected, the minimum-size criterion won't be satisfied.
		| otherwise			= Data.FileCombination.singleton fileSizeAndPath : foldr binaryChoice [] (
			nonEmptyCombinations (minimumBytes - Data.File.getSize fileSizeAndPath) remainder	-- Recurse.
		)
		where
			binaryChoice :: Data.FileCombination.FileCombination -> [Data.FileCombination.FileCombination] -> [Data.FileCombination.FileCombination]
			binaryChoice combinationExcluding
				| Data.FileCombination.hasSizeBy (<= maximumCombinationSize) combinationIncluding	= (combinationExcluding :) . (combinationIncluding :)
				| otherwise										= (combinationExcluding :)
				where
					combinationIncluding :: Data.FileCombination.FileCombination
					combinationIncluding	= Data.FileCombination.prepend fileSizeAndPath combinationExcluding

-- | Orders the files by decreasing size, calls 'findCombinations', calls 'Data.FileCombination.risingFilter' to select progressively better solutions.
findBestFit
	:: Factory.Data.Interval.Interval Data.File.FileSize	-- ^ The closed interval of acceptible sizes for file-combinations.
	-> [Data.File.FileSizeAndPath]				-- ^ The input list of file-names & sizes.
	-> [Data.FileCombination.FileCombination]		-- ^ A reduced list of increasingly suitable file-combinations.
findBestFit solutionSizeBounds	= Data.FileCombination.risingFilter (Factory.Data.Interval.getMinBound solutionSizeBounds) . findCombinations solutionSizeBounds . Data.File.orderByDecreasingSize {-which makes findCombinations faster-}

{- |
	* Recursively bisects the task, distributing the sub-tasks to 'findBestFit', to utilise the available CPU-cores.

	* The task is bisected by removing the smallest file, then solving the remaining problem for two independent cases; that the selected file is excluded or the selected file is included, in the final solution.
	Selecting the smallest file rather than the largest, seems to balance the load of the sub-tasks more evenly.
	CAVEAT: no account has been taken of the possibility that the smallest file has size zero, which makes the sub-tasks identical.

	* Recombines the part solutions to finds the single monotonically increasing list of file-combinations matching the original criteria.

	* CAVEAT: whilst the ultimate solution is similar, regardless of the specified number of CPU-cores available, the path leading to it typically differs.
-}
distributeAndFindBestFit
	:: RealFrac ratio
	=> Data.CommandOptions.CommandOptions ratio
	-> [Data.File.FileSizeAndPath]	-- ^ The unordered list of files & sizes.
	-> IO [Data.FileCombination.FileCombination]
distributeAndFindBestFit commandOptions fileSizeAndPathList	= let
	slave _ _ []						= return {-to IO-monad-} [Data.FileCombination.nullFileCombination]
	slave 1	commandOptions' increasingFileSizeAndPathList	= let
		solutionSizeBounds	= Data.CommandOptions.solutionSizeBounds commandOptions'
	 in do
		Control.Monad.when (Data.CommandOptions.getVerbosity commandOptions' == maxBound) . System.IO.hPutStrLn System.IO.stderr $ "INFO: acceptable file-size interval " ++ show solutionSizeBounds ++ " bytes."

		return {-to IO-monad-} $ findBestFit solutionSizeBounds increasingFileSizeAndPathList	-- Delegate the task to the single-threaded algorithm.
	slave numCapabilities' commandOptions' (selectedFileSizeAndPath {-the smallest-} : remainingFileSizeAndPaths)	= let
		recurse	= slave $ numCapabilities' `div` 2	-- Partially apply.
	 in do
		Control.Monad.when (Data.CommandOptions.getVerbosity commandOptions' == maxBound) . System.IO.hPutStrLn System.IO.stderr $ "INFO: " ++ show numCapabilities' ++ " CPU-cores => bisecting task into those, with & without " ++ show selectedFileSizeAndPath ++ "."

		fileCombinationsExcludingSelected	<- recurse commandOptions' {-i.e the original space-} remainingFileSizeAndPaths	-- This is about half the total task.

		let
			commandOptions''	= Data.CommandOptions.subtractFile (Data.File.getSize selectedFileSizeAndPath) commandOptions' -- Reduce the requirements by the size of the selected file.

		if Data.CommandOptions.getMaximumBytes commandOptions'' < 0
			then return {-to IO-monad-} fileCombinationsExcludingSelected	-- The selected file won't fit.
			else map (
				Data.FileCombination.prepend selectedFileSizeAndPath	-- Prepend the selected file to all file-combinations.
			) `fmap` recurse commandOptions'' remainingFileSizeAndPaths >>= Control.Concurrent.DivideAndConquer.divideAndConquer Data.FileCombination.risingMergeByAggregateFileSize fileCombinationsExcludingSelected	-- Merge the part-solutions.
 in Control.Concurrent.getNumCapabilities >>= (\numCapabilities -> slave numCapabilities commandOptions $ Data.File.orderByIncreasingSize fileSizeAndPathList)

{- |
	* Neither 'distributeAndFindBestFit' nor 'findBestFit' distinguish between empty & non-empty files,
	but empty files cause significant inefficiency in the former (where the same calculation is performed multiple times)
	& could be treated much more efficiently in the latter (since they're potentially a member of any other solution).

	* This function side-lines empty files, delegates the remaining problem to 'distributeAndFindBestFit' (& consequently 'findBestFit'),
	then prepends combinations of empty files to the resulting combinations of non-empty files.
-}
partitionEmptyFilesAndDistributeAndFindBestFit
	:: RealFrac ratio
	=> Data.CommandOptions.CommandOptions ratio
	-> [Data.File.FileSizeAndPath]	-- ^ The unordered list of files & sizes.
	-> IO [Data.FileCombination.FileCombination]
partitionEmptyFilesAndDistributeAndFindBestFit commandOptions fileSizeAndPathList
	| Data.CommandOptions.getIncludeEmpty commandOptions	= do
		printStatistics fileSizeAndPathList

		concatMap (
			\fileCombination -> map (
				\emptyFilePathCombination	-> fileCombination {
					Data.FileCombination.getFilePathList	= emptyFilePathCombination ++ Data.FileCombination.getFilePathList fileCombination
				}
			) $ Data.List.subsequences {-find all combinations-} emptyFiles
		 ) `fmap` nonEmptyFileCombinations
	| otherwise {-exclude empty files-}			= do
		Control.Monad.unless (Data.CommandOptions.getVerbosity commandOptions == minBound || null emptyFiles) . System.IO.hPutStrLn System.IO.stderr $ "WARNING: rejecting empty files; " ++ show emptyFiles ++ "."

		Control.Monad.when (null nonEmptyFilePathAndSizeList) $ error "there are zero non-empty files."

		printStatistics nonEmptyFilePathAndSizeList

		nonEmptyFileCombinations
	where
		printStatistics l				= Control.Monad.unless (Data.CommandOptions.getVerbosity commandOptions == minBound || null l) . System.IO.hPutStrLn System.IO.stderr $ "INFO: file-(count, aggregate size, mean, standard-deviation); " ++ show (Data.File.getFileSizeStatistics l :: (Int, Data.File.FileSize, Float, Float)) ++ "."
		(emptyFiles, nonEmptyFilePathAndSizeList)	= Control.Arrow.first (map Data.File.getPath) $ Data.List.partition (Data.File.hasSizeBy (== 0)) fileSizeAndPathList
		nonEmptyFileCombinations			= distributeAndFindBestFit commandOptions nonEmptyFilePathAndSizeList	-- Delegate.

