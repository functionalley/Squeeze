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

 [@DESCRIPTION@]	Defines file-related type-synonyms, and associated operations.
-}

module Squeeze.Data.File(
-- * Types
-- ** Type-synonyms
	FilePathList,
	FileSize,
	FileSizeAndPath,
-- * Functions
	accumulateSize,
	aggregateSize,
--	expandDirectory,
	findDuplicates,
--	findSize,
	findSizes,
	orderByIncreasingSize,
	orderByDecreasingSize,
	getFileSizeStatistics,
	selectSuitableFileSizes,
-- ** Accessors
	getSize,
	getPath,
-- ** Predicates
	hasSizeBy
) where

import			Control.Arrow((&&&))
import qualified	Control.Monad
import qualified	Control.Monad.Writer
import qualified	Data.List
import qualified	Data.Ord
import qualified	Factory.Math.Statistics
import qualified	System.Directory
import			System.FilePath((</>))
import qualified	System.IO
import qualified	ToolShed.Data.Foldable

-- | A type suitable for containing an arbitrary set of file-paths.
type FilePathList	= [System.IO.FilePath]

-- | A type-synonym specifically to hold file-sizes (in bytes).
type FileSize		= Integer	-- Matches the return-type of 'IO.hFileSize'.

-- | A type suitable for containing a file-path, qualified by the corresponding 'FileSize'.
type FileSizeAndPath	= (FileSize, System.IO.FilePath)

-- | Accessor.
getSize :: FileSizeAndPath -> FileSize
getSize	= fst

-- | Accessor.
getPath :: FileSizeAndPath -> System.IO.FilePath
getPath	= snd

-- | Sum the 'FileSize's in the specified list.
aggregateSize :: [FileSizeAndPath] -> FileSize
aggregateSize	= foldr ((+) . getSize) 0

{- |
	* Returns the cumulative sequence of sizes, as each file is prepended to the specified list.

	* CAVEAT: the list-length is one greater than that supplied, since the last element represents the size with zero files.
-}
accumulateSize :: [FileSizeAndPath] -> [FileSize]
accumulateSize	= scanr ((+) . getSize) 0

{- |
	* Recursively descend the specified path, accumulating a list of files.

	* CAVEAT: all non-directory files are returned; devices, pipes, sockets, symlinks ...
-}
expandDirectory :: System.IO.FilePath -> IO FilePathList
expandDirectory filePath	= do
	directoryExists	<- System.Directory.doesDirectoryExist filePath

	if directoryExists
		then System.Directory.getDirectoryContents filePath >>= fmap concat . mapM (
			expandDirectory {-recurse-} . (filePath </>) {-qualify the path-}
		) . filter (
			`notElem` [".", ".."]	-- Prevent infinite recursion.
		)
		else {-non-directory-} return {-to IO-monad-} [filePath]	-- CAVEAT: this could include non-existent paths, devices, pipes, sockets, symlinks ...

{- |
	* Finds any file-paths which have been specified more than once.

	* This includes files which have been implicitly specified via a directory.
-}
findDuplicates :: FilePathList -> IO FilePathList
findDuplicates	= fmap (map head . filter ((> 1) . length) . ToolShed.Data.Foldable.gather . concat) . mapM expandDirectory

{- |
	* Get the size of a file, treating a directory as an atomic unit.

	* CAVEAT: the size of a symlink, is that of the file to which it refers.
-}
findSize :: System.IO.FilePath -> IO FileSize
findSize filePath	= expandDirectory filePath >>= fmap aggregateSize . mapM (\f -> flip (,) f `fmap` System.IO.withFile f System.IO.ReadMode System.IO.hFileSize)

-- | Finds file-sizes.
findSizes :: FilePathList -> IO [FileSizeAndPath]
findSizes	= uncurry fmap . (flip zip &&& mapM findSize)

-- | Sorts a list of 'FileSizeAndPath' by increasing size; ie. smallest first.
orderByIncreasingSize :: [FileSizeAndPath] -> [FileSizeAndPath]
orderByIncreasingSize	= Data.List.sortBy $ Data.Ord.comparing getSize

-- | Sorts a list of 'FileSizeAndPath' by decreasing size; ie. smallest first.
orderByDecreasingSize :: [FileSizeAndPath] -> [FileSizeAndPath]
orderByDecreasingSize	= reverse . orderByIncreasingSize

-- | True if the specified file has the required size according to the specified predicate.
hasSizeBy
	:: (FileSize -> Bool)	-- ^ The predicate.
	-> FileSizeAndPath	-- ^ The file-parameters to be tested.
	-> Bool
hasSizeBy predicate	= predicate . getSize

-- | Acquire statistics related to a list of files.
getFileSizeStatistics
	:: (Fractional mean, Floating standardDeviation)
	=> [FileSizeAndPath]
	-> (Int, FileSize, mean, standardDeviation)	-- ^ (Number of components, Aggregate size, Mean size, Standard-deviation).
getFileSizeStatistics l	= (
	length l,
	sum sizes,
	Factory.Math.Statistics.getMean sizes,
	Factory.Math.Statistics.getStandardDeviation sizes
 ) where
	sizes	= map getSize l

{- |
	* Partitions the specified list of file-sizes & paths, into those whose size is suitable according to the specified predicate & those which are unsuitable.

	* Logs the results.
-}
selectSuitableFileSizes :: (FileSize -> Bool) -> [FileSizeAndPath] -> Control.Monad.Writer.Writer [String] [FileSizeAndPath]
selectSuitableFileSizes predicate fileSizeAndPathList	= let
	(accepted, rejected)	= Data.List.partition (hasSizeBy predicate) fileSizeAndPathList
 in do
	Control.Monad.unless (null rejected) $ Control.Monad.Writer.tell ["WARNING: rejecting files of unsuitable size; " ++ show rejected ++ "."]

	return {-to Writer-monad-} accepted

