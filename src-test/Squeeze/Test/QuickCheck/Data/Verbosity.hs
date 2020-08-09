{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2013 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary'.
-}

module Squeeze.Test.QuickCheck.Data.Verbosity() where

import qualified	Squeeze.Data.Verbosity	as Data.Verbosity
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Data.Verbosity.Verbosity	where
	arbitrary	= Test.QuickCheck.elements Data.Verbosity.range
