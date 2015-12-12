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

 [@DESCRIPTION@]	Defines tests to dynamically verify the module "Squeeze.Squeeze" against arbitrary data.
-}

module Squeeze.Test.QuickCheck.Squeeze(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Factory.Data.Interval
import qualified	Squeeze.Data.File		as Data.File
import qualified	Squeeze.Data.FileCombination	as Data.FileCombination
import qualified	Squeeze.Squeeze			as Squeeze
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.Foldable
import qualified	ToolShed.Data.List

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [prop_totalCombinations, prop_fileSizeBounds, prop_allFiles, prop_monotonicFileSize, prop_uniqueFileNames]	where
	mkFileSizeAndPathList :: (Integral i, Show i) => [i] -> [Data.File.FileSizeAndPath]
	mkFileSizeAndPathList	= take 12 . Data.List.nubBy (ToolShed.Data.List.equalityBy snd) . map (toInteger . abs &&& show)	-- CAVEAT: may be shorter than requested.

	prop_totalCombinations, prop_fileSizeBounds, prop_allFiles, prop_monotonicFileSize, prop_uniqueFileNames :: [Integer] -> Test.QuickCheck.Property
	prop_totalCombinations integers =  Test.QuickCheck.label "prop_totalCombinations" $ (length . Data.List.nub . Squeeze.findCombinations (0, fromIntegral (maxBound :: Int)) $ Data.File.orderByDecreasingSize fileSizeAndPathList) == 2 ^ length fileSizeAndPathList where
		fileSizeAndPathList	= mkFileSizeAndPathList integers

	prop_fileSizeBounds integers	=  Test.QuickCheck.label "prop_fileSizeBounds" . all ((`Factory.Data.Interval.elem'` fileSizeBounds) . Data.FileCombination.getAggregateFileSize) . Squeeze.findCombinations fileSizeBounds $ Data.File.orderByDecreasingSize fileSizeAndPathList	where
		fileSizeAndPathList	= mkFileSizeAndPathList integers

		fileSizeBounds :: Factory.Data.Interval.Interval Data.File.FileSize
		fileSizeBounds	= (minimumBytes, maximumBytes)	where
			maximumBytes, minimumBytes :: Data.File.FileSize
			maximumBytes	= round $ fromIntegral (Data.File.aggregateSize fileSizeAndPathList) / (4.0 :: Double)	-- Arbitrarily.
			minimumBytes	= maximumBytes `div` 2	-- Arbitrarily.

	prop_allFiles integers	= Test.QuickCheck.label "prop_allFiles" . (== aggregateSize) . last . map Data.FileCombination.getAggregateFileSize $ Squeeze.findBestFit (Factory.Data.Interval.precisely aggregateSize) fileSizeAndPathList	where
		fileSizeAndPathList	= mkFileSizeAndPathList integers

		aggregateSize :: Data.File.FileSize
		aggregateSize	= Data.File.aggregateSize fileSizeAndPathList

	prop_monotonicFileSize integers	= Test.QuickCheck.label "prop_monotonicFileSize" . uncurry (==) . (id &&& Data.List.sort {-stable-}) . map Data.FileCombination.getAggregateFileSize $ Squeeze.findBestFit (0, fromIntegral (maxBound :: Int)) fileSizeAndPathList	where
		fileSizeAndPathList	= mkFileSizeAndPathList integers

	prop_uniqueFileNames integers	= Test.QuickCheck.label "prop_uniqueFileNames" . all (
		all ((== 1) . length) . ToolShed.Data.Foldable.gather . Data.FileCombination.getFilePathList
	 ) $ Squeeze.findBestFit (0, fromIntegral (maxBound :: Int)) fileSizeAndPathList	where
		fileSizeAndPathList	= mkFileSizeAndPathList integers

