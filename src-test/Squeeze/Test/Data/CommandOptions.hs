{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2013-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'Data.CommandOptions.CommandOptions'.
-}

module Squeeze.Test.Data.CommandOptions(
-- * Types
-- ** Type-synonyms
	CommandOptions
) where

import qualified	Test.QuickCheck
import qualified	Squeeze.Data.CommandOptions	as Data.CommandOptions
import			Squeeze.Test.Data.Verbosity()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Enum				ratio,
	Fractional			ratio,
	Ord				ratio,
	Real				ratio,
	Test.QuickCheck.Arbitrary	ratio
 ) => Test.QuickCheck.Arbitrary (Data.CommandOptions.CommandOptions ratio)	where
	arbitrary	= Data.CommandOptions.mkCommandOptions <$> Test.QuickCheck.arbitrary {-includeEmpty-} <*> (
		abs <$> Test.QuickCheck.arbitrary			-- maximumBytes.
	 ) <*> Test.QuickCheck.arbitrary {-maybeRandomSeed-} <*> (
		recip . succ . abs <$> Test.QuickCheck.arbitrary	-- minimumUsageRatio.
	 ) <*> Test.QuickCheck.arbitrary {-verbosity-}

-- | Defines a concrete type for testing.
type CommandOptions	= Data.CommandOptions.CommandOptions Double

