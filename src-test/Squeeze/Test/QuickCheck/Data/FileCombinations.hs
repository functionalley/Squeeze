{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Defines tests to dynamically verify the module "Squeeze.Data.FileCombinations" against arbitrary data.
-}

module Squeeze.Test.QuickCheck.Data.FileCombinations(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Squeeze.Data.FileCombination	as Data.FileCombination
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Data.FileCombination.FileCombination	where
	arbitrary	= do
		filePathList		<- Data.List.nub `fmap` (Test.QuickCheck.elements [0 .. 99] >>= Test.QuickCheck.vector)
		aggregateFileSize	<- Test.QuickCheck.elements [0 .. fromIntegral $ length filePathList]	-- Assume files of size either zero or one byte.

		return $ Data.FileCombination.mkFileCombination aggregateFileSize filePathList

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [prop_risingFilter, prop_risingFilter', prop_risingMergeByAggregateFileSize]	where
	prop_risingFilter, prop_risingFilter', prop_risingMergeByAggregateFileSize :: [Data.FileCombination.FileCombination] -> Test.QuickCheck.Property
	prop_risingFilter	= Test.QuickCheck.label "prop_risingFilter" . uncurry (==) . (id &&& Data.List.sort) . map Data.FileCombination.getAggregateFileSize . Data.FileCombination.risingFilter 0
	prop_risingFilter'	= Test.QuickCheck.label "prop_risingFilter'" . uncurry (==) . (id &&& Data.FileCombination.risingFilter 0) . Data.List.sortBy Data.FileCombination.comparingAggregateFileSize

	prop_risingMergeByAggregateFileSize	= Test.QuickCheck.label "prop_risingMergeByAggregateFileSize" . uncurry (==) . (
		id &&& map head . Data.List.group . uncurry Data.FileCombination.risingMergeByAggregateFileSize . (id &&& id)
	 ) . Data.FileCombination.risingFilter 0

