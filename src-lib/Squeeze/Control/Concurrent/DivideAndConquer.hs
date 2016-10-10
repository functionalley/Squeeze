{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]

	Provides the capability to write two unevaluated lists to separate concurrent channels,
	read them both & merge the results into a single list, evaluating in parallel, lazily on demand.
-}

module Squeeze.Control.Concurrent.DivideAndConquer(
-- * Types
-- ** Type-synonyms
--	TerminatedChannel,
-- * Functions
--	writeListToChan,
--	readListFromChan,
	divideAndConquer
) where

import qualified	Control.Concurrent
import qualified	Control.Concurrent.Chan
import qualified	System.IO.Unsafe
import qualified	Control.Monad
import qualified	Data.Maybe

#if !defined(MIN_VERSION_base) || !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

-- | A channel terminated by a sentinel.
type TerminatedChannel a	= Control.Concurrent.Chan.Chan (Maybe a)

-- | Write the specified list to the specified channel, terminating it with a sentinel.
writeListToChan :: TerminatedChannel a -> [a] -> IO ()
writeListToChan chan l	= mapM_ (Control.Concurrent.Chan.writeChan chan . Just) l >> Control.Concurrent.Chan.writeChan chan Nothing {-sentinel-}

{- |
	* Read the contents of the specified channel, up to the sentinel.

	* Any attempt to read beyond the sentinel, will block forever since the writer is out of data.
-}
readListFromChan :: TerminatedChannel a -> IO [a]
readListFromChan chan	= System.IO.Unsafe.unsafeInterleaveIO {-read lazily, on demand-} $ Control.Concurrent.Chan.readChan chan >>= Data.Maybe.maybe (
	return {-to IO-monad-} []
 ) (
	\x	-> (x :) <$> readListFromChan chan {-recurse-}
 )

-- | Writes the two unevaluated lists to separate concurrent channels, lazily reads them both (resulting in parallel evaluation) & merges them into a single results-list.
divideAndConquer
	:: ([a] -> [a] -> [a])	-- ^ Merge-function, which reads from two channels, to produce a third.
	-> [a]			-- ^ Data for first channel.
	-> [a]			-- ^ Data for second channel.
	-> IO [a]
divideAndConquer merge l r	= do
	chan0	<- Control.Concurrent.Chan.newChan
	chan1	<- Control.Concurrent.Chan.newChan

	Control.Monad.void . Control.Concurrent.forkIO $ writeListToChan chan0 l
	Control.Monad.void . Control.Concurrent.forkIO $ writeListToChan chan1 r

	merge <$> readListFromChan chan0 <*> readListFromChan chan1

