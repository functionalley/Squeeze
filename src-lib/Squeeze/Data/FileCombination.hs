{-
	Copyright (C) 2010-2016 Dr. Alistair Ward

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

 [@DESCRIPTION@]	A data-type which references a set of files by their paths, and qualifies them with their aggregate size.
-}

module Squeeze.Data.FileCombination(
-- * Types
-- ** Data-types
	FileCombination(
--		MkFileCombination,
		getAggregateFileSize,
		getFilePathList
	),
-- * Constants
	nullFileCombination,
-- * Functions
	comparingAggregateFileSize,
	prepend,
	risingFilter,
--	risingMerge,
	risingMergeByAggregateFileSize,
-- ** Constructors
	mkFileCombination,
	singleton,
-- ** Predicates
	hasSizeBy
) where

import qualified	Data.List
import qualified	Data.Ord
import qualified	Squeeze.Data.File	as Data.File
import qualified	ToolShed.Data.Foldable
import qualified	ToolShed.SelfValidate

-- | Declare a list of files qualified by its aggregate size.
data FileCombination	= MkFileCombination {
	getAggregateFileSize	:: !Data.File.FileSize,		-- ^ The aggregate size of the files referenced by 'getFilePathList'.
	getFilePathList		:: Data.File.FilePathList	-- ^ A list of paths, defining a set of files.
} deriving Eq

instance Show FileCombination where
	showsPrec _ fileCombination	= shows (getAggregateFileSize fileCombination) . showChar '\t' . shows (Data.List.sort $ getFilePathList fileCombination)

instance ToolShed.SelfValidate.SelfValidator FileCombination	where
	getErrors fileCombination	= ToolShed.SelfValidate.extractErrors [
		(
			getAggregateFileSize fileCombination < 0,
			"aggregate size must be positive; " ++ show fileCombination
		), (
			getAggregateFileSize fileCombination /= 0 && null (getFilePathList fileCombination),
			"when zero files are specified, the aggregate size must also be zero; " ++ show fileCombination
		), let
			duplicateFilePaths	= map head . filter ((> 1) . length) . ToolShed.Data.Foldable.gather $ getFilePathList fileCombination
		in (
			not $ null duplicateFilePaths,
			"duplicate file-paths have been specified; " ++ show duplicateFilePaths
		)
	 ]

-- | A constant empty instance.
nullFileCombination :: FileCombination
nullFileCombination	= MkFileCombination 0 []

-- | Smart constructor.
mkFileCombination :: Data.File.FileSize -> Data.File.FilePathList -> FileCombination
mkFileCombination fileSize filePathList
	| ToolShed.SelfValidate.isValid fileCombination	= fileCombination
	| otherwise					= error $ "Squeeze.Data.FileCombination.mkFileCombination:\t" ++ ToolShed.SelfValidate.getFirstError fileCombination
	where
		fileCombination	= MkFileCombination fileSize filePathList

-- | Construct a 'FileCombination' from a single 'Data.File.FileSizeAndPath'.
singleton ::  Data.File.FileSizeAndPath -> FileCombination
singleton (fileSize, filePath)	= mkFileCombination fileSize [filePath]

{- |
	* Prepend a 'Data.File.FileSizeAndPath' to an existing 'FileCombination'.

	* CAVEAT: performance hot-spot, so it by-passes the checks made by 'mkFileCombination'.
-}
{-# INLINE prepend #-}
prepend
	:: Data.File.FileSizeAndPath	-- ^ The new path to prepend to the incumbent file-combination.
	-> FileCombination		-- ^ The incumbent combination of files.
	-> FileCombination
prepend (fileSize, filePath) MkFileCombination {
	getAggregateFileSize	= aggregateFileSize,
	getFilePathList		= filePathList
} = MkFileCombination {
	getAggregateFileSize	= fileSize + aggregateFileSize,
	getFilePathList		= filePath : filePathList
}

-- | Predicate used to determine whether a specific file-combination matches a size-related requirement.
{-# INLINE hasSizeBy #-}
hasSizeBy
	:: (Data.File.FileSize -> Bool)	-- ^ The predicate.
	-> FileCombination		-- ^ The input datum to be tested.
	-> Bool
hasSizeBy predicate MkFileCombination { getAggregateFileSize = aggregateFileSize }	= predicate aggregateFileSize

-- | Progressively raises the selection-criterion as each match is found, to produce monotonically increasing file-combinations.
risingFilter
	:: Data.File.FileSize	-- ^ The initial minimum byte-size of file to accept.
	-> [FileCombination]	-- ^ The input list of files to filter.
	-> [FileCombination]	-- ^ The resulting list of files, which have met rising criterion.
risingFilter _ []	= []
risingFilter minimumSize (fileCombination@MkFileCombination { getAggregateFileSize = aggregateFileSize } : fileCombinations)
	| aggregateFileSize >= minimumSize	= fileCombination : risingFilter aggregateFileSize fileCombinations
	| otherwise				= risingFilter minimumSize fileCombinations

{- |
	* Merges two lists of monotonically increasing values, into a single monotonically increasing list, by dropping values which compare less than results already found.

	* CAVEAT: both lists must produce an element, in order to determine which is selected.
	As a result, though one list produces a value, it can't be returned until the other does (which make take a long time), even if ultimately the first is then selected.
-}
risingMerge
	:: (FileCombination -> FileCombination -> Ordering)	-- ^ Comparator used to select the best file-combination from the heads of the two list supplied.
	-> [FileCombination]					-- ^ A list of monotonically increasing file-combinations.
	-> [FileCombination]					-- ^ A list of monotonically increasing file-combinations.
	-> [FileCombination]
risingMerge cmp	= slave nullFileCombination	where
	lessThan bar	= (== LT) . (`cmp` bar)

	slave bar [] r			= dropWhile (lessThan bar) r
	slave bar l []			= dropWhile (lessThan bar) l
	slave _ (x : xs) (y : ys)	= case x `cmp` y of
		GT	-> x : slave x xs ys
		LT	-> y : slave y xs ys
		_	-> x : y : slave x xs ys

-- | Compares two /file-combination/s by their aggregate file-size.
comparingAggregateFileSize :: FileCombination -> FileCombination -> Ordering
comparingAggregateFileSize	= Data.Ord.comparing getAggregateFileSize

{- |
	Merges two lists of monotonically increasing lists of file-combinations,
	into a single monotonically increasing list,
	by dropping values which have a smaller aggregate size than results already found.
-}
risingMergeByAggregateFileSize
	:: [FileCombination]	-- ^ A list of monotonically increasing file-combinations.
	-> [FileCombination]	-- ^ A list of monotonically increasing file-combinations.
	-> [FileCombination]
risingMergeByAggregateFileSize	= risingMerge comparingAggregateFileSize
