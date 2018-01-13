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

	* Defines options for program-operation.

	* Defines an appropriate default value, which is expected to be over-ridden on the command-line.

	* Self-validates.
-}

module Squeeze.Data.CommandOptions(
-- * Types
-- ** Data-types
	CommandOptions(
--		MkCommandOptions,
		getIncludeEmpty,
		getMaybeRandomSeed,
		getMaximumBytes,
		getMinimumUsageRatio,
		getVerbosity
	),
-- * Functions
	solutionSizeBounds,
	deriveMinimumBytes,
	subtractFile,
-- ** Constructor
	mkCommandOptions
) where

import			Control.Arrow((&&&))
import qualified	Data.Default
import qualified	Distribution.Verbosity
import qualified	Factory.Data.Interval
import qualified	Squeeze.Data.File	as Data.File
import qualified	ToolShed.SelfValidate

-- | Declares a record to contain command-line options.
data CommandOptions ratio	= MkCommandOptions {
	getIncludeEmpty		:: Bool,				-- ^ Whether empty directories or files should be included in any solution.
	getMaximumBytes		:: Data.File.FileSize,			-- ^ The maximum space (in bytes) available in which to store a subset of the specified files.
	getMaybeRandomSeed	:: Maybe Int,				-- ^ Optionally seed the random-number generator to produce a repeatable pseudo-random sequence.
	getMinimumUsageRatio	:: ratio,				-- ^ The minimum acceptable usage-ratio of 'getMaximumBytes'.
	getVerbosity		:: Distribution.Verbosity.Verbosity	-- ^ Set the threshold for ancillary information-output.
} deriving (Eq, Show)

instance Fractional f => Data.Default.Default (CommandOptions f)	where
	def = MkCommandOptions {
		getIncludeEmpty		= False,
		getMaximumBytes		= 4700000000,	-- DVD-size; just under 4.4GiB.
		getMaybeRandomSeed	= Nothing,
		getMinimumUsageRatio	= 9 / 10,	-- 90% full.
		getVerbosity		= Distribution.Verbosity.normal
	}

instance Real ratio => ToolShed.SelfValidate.SelfValidator (CommandOptions ratio)	where
	getErrors commandOptions	= map snd $ filter (($ commandOptions) . fst) [
		((< 0) . getMaximumBytes,	"invalid maximumBytes; " ++ show (getMaximumBytes commandOptions)),
		((< 0) . getMinimumUsageRatio,	"invalid minimumUsageRatio; " ++ show (realToFrac $ getMinimumUsageRatio commandOptions :: Double)),
		((> 1) . getMinimumUsageRatio,	"invalid minimumUsageRatio; " ++ show (realToFrac $ getMinimumUsageRatio commandOptions :: Double))
	 ]

-- | Smart constructor.
mkCommandOptions
	:: Real ratio
	=> Bool
	-> Data.File.FileSize
	-> Maybe Int
	-> ratio
	-> Distribution.Verbosity.Verbosity
	-> CommandOptions ratio
mkCommandOptions includeEmpty maximumBytes maybeRandomSeed minimumUsageRatio verbosity
	| ToolShed.SelfValidate.isValid commandOptions	= commandOptions
	| otherwise					= error $ "Squeeze.Data.CommandOptions.mkCommandOptions:\t" ++ ToolShed.SelfValidate.getFirstError commandOptions
	where
		commandOptions	= MkCommandOptions includeEmpty maximumBytes maybeRandomSeed minimumUsageRatio verbosity

-- | Derives the minimum number of bytes, from other options.
deriveMinimumBytes :: RealFrac ratio => CommandOptions ratio -> Data.File.FileSize
deriveMinimumBytes	= floor . uncurry (*) . (getMinimumUsageRatio &&& realToFrac . getMaximumBytes)

-- | Reduce the requirements by the specified file-size.
subtractFile
	:: RealFrac ratio
	=> Data.File.FileSize
	-> CommandOptions ratio
	-> CommandOptions ratio
subtractFile fileSize commandOptions
	| fileSize < 0		= error $ "Squeeze.Data.CommandOptions.subtractFile:\tnegative file-size=" ++ show fileSize
	| maximumBytes' < 0	= error $ "Squeeze.Data.CommandOptions.subtractFile:\tfile-size=" ++ show fileSize ++ " > maximum=" ++ show maximumBytes
	| otherwise		= commandOptions {
		getMaximumBytes		= maximumBytes',
		getMinimumUsageRatio	= if maximumBytes' == 0
			then 0	-- CAVEAT: the calculation below would otherwise attempt to evaluate 0 / 0.
			else fromIntegral (max 0 $ deriveMinimumBytes commandOptions - fileSize) / fromIntegral maximumBytes'
	} where
		maximumBytes, maximumBytes' :: Data.File.FileSize
		maximumBytes	= getMaximumBytes commandOptions
		maximumBytes'	= maximumBytes - fileSize

-- | The bounds on the aggregate size of the set of files.
solutionSizeBounds :: RealFrac f => CommandOptions f -> Factory.Data.Interval.Interval Data.File.FileSize
solutionSizeBounds	= deriveMinimumBytes &&& getMaximumBytes

