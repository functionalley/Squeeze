{-# LANGUAGE CPP #-}
#ifdef MIN_TOOL_VERSION_ghc
#if MIN_TOOL_VERSION_ghc(8,0,1)
{-# OPTIONS_GHC -freduction-depth=25 #-}
#endif
#else
{-# OPTIONS_GHC -fcontext-stack=25 #-}
#endif
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

	* Contains the entry-point of the application.

	* Processes the command-line arguments.

	* Delegates the task to 'Squeeze.findBestFit', potentially on multiple threads.
-}

module Main(main) where

import qualified	Control.Monad
import qualified	Control.Monad.Writer
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Version
import qualified	Distribution.Package
import qualified	Distribution.Text
import qualified	Distribution.Verbosity
import qualified	Distribution.Version
import qualified	Factory.Math.Probability
import qualified	Paths_squeeze			as Paths	-- Either local stub, or package-instance autogenerated by 'Setup.hs build'.
import qualified	Squeeze.Data.CommandOptions	as Data.CommandOptions
import qualified	Squeeze.Data.File		as Data.File
import qualified	Squeeze.Squeeze			as Squeeze
import qualified	Squeeze.Test.Performance	as Test.Performance
import qualified	System.Console.GetOpt		as G
import qualified	System.Environment
import qualified	System.Exit
import qualified	System.FilePath
import qualified	System.Info
import qualified	System.IO
import qualified	System.IO.Error
import qualified	Text.Printf
import qualified	ToolShed.Data.List
import qualified	ToolShed.SelfValidate
import qualified	ToolShed.System.TimeAction

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

-- | Coerce the polymorphic data-type to concrete instance, in order that it's fields may be read from the command-line.
type CommandOptions'	= Data.CommandOptions.CommandOptions Rational	-- 'Double' would also be a suitable type-parameter.

-- | Used to thread user-defined command-line options, though the list of functions which implement them.
type CommandLineAction	= CommandOptions' -> IO CommandOptions'	-- Supplied as the type-argument to 'G.OptDescr'.

-- | On failure to parse the specified string, returns an explanatory error.
read' :: Read a => String -> String -> a
read' errorMessage s	= case reads s of
	[(x, "")]	-> x
	_		-> error $ errorMessage ++ show s

-- | On failure to parse a command-line argument, returns an explanatory error.
readCommandArg :: Read a => String -> a
readCommandArg	= read' "failed to parse command-line argument "

-- | Reads a bounded integral from the command-line, guarding against overflow.
readBoundedIntegral :: Integral i => String -> i
readBoundedIntegral s
	| fromIntegral bounded /= unbounded	= error $ "integral value exceeds permissible bounds; " ++ show unbounded ++ "."
	| otherwise				= bounded
	where
		unbounded	= readCommandArg s
		bounded		= fromInteger unbounded

{- |
	* Parses the command-line arguments, to determine 'Data.CommandOptions.CommandOptions', which over-ride the default value.

	* Any arguments which follow known 'Data.CommandOptions.CommandOptions',
	are interpreted as file-names to consider when attempting to find a suitable fit for the specified space-constraints.

	* If the specified file-name is /-/, then the actual file-names are read from /standard input/, to augment any other non-options specified.

	* Delegates the donkey-work to 'partitionEmptyFilesAndDistributeAndFindBestFit'.
	Because this may take a long time, it prints the results in real time, rather than batching until the optimum has been determined.
-}
main :: IO ()
main	= do
	progName	<- System.Environment.getProgName

	let
		defaultCommandOptions :: CommandOptions'
		defaultCommandOptions	= Data.Default.def

		defaultRandomSeed :: Int
		defaultRandomSeed	= 0

		optDescrList :: [G.OptDescr CommandLineAction]
		optDescrList	= [
--				 String	[String]		(G.ArgDescr CommandLineAction)			String
			G.Option "?"	["help"]		(G.NoArg $ const printUsage)			"Display this help, & then exit.",
			G.Option ""	["verbosity"]		(
				setVerbosity `G.ReqArg` ToolShed.Data.List.showListWith listDelimiters [minBound :: Distribution.Verbosity.Verbosity .. maxBound] ""
			)											("Define the log-level; default '" ++ show (Data.CommandOptions.getVerbosity defaultCommandOptions) ++ "'. CAVEAT: to be effective, it must precede other options."
			),
			G.Option "v"	["version"]		(G.NoArg $ const printVersion)			"Print version-information, & then exit.",
			G.Option "z"	["includeEmpty"]	(setIncludeEmpty `G.OptArg` "<Bool>")		("Whether empty files & directories may be included in any solution; default '" ++ show (Data.CommandOptions.getIncludeEmpty defaultCommandOptions) ++ "'."),
			G.Option "M"	["maximumBytes"]	(setMaximumBytes `G.ReqArg` "<Int>")		("The maximum bytes of available space; default '" ++ show defaultMaximumBytes ++ "'."),
			G.Option "m"	["minimumUsageRatio"]	(setMinimumUsageRatio `G.ReqArg` "<Float>")	("The minimum acceptable space usage-ratio; default '" ++ show (realToFrac $ Data.CommandOptions.getMinimumUsageRatio defaultCommandOptions :: Double) ++ "'."),
			G.Option "r"	["randomSeed"]		(G.OptArg setRandomSeed "<Int>")		("Seed the random number-generator with the specified integer, to produce a repeatable pseudo-random sequence as required for performance-testing. If this option is unspecified then the seed is unpredictable, but if only its argument is unspecified then the seed defaults to '" ++ show defaultRandomSeed ++ "'. CAVEAT: to be effective, it must precede either 'testPerformanceContinuous' or 'testPerformanceDiscrete'."),
			G.Option ""	["testPerformanceContinuous"]	(
				testPerformanceContinuous `G.ReqArg` "(<Int>, <ContinuousDistribution>)"
			)											"Measure the CPU-seconds (accumulated over all CPU-cores) required to find the best fit, for the specified number of randomly generated virtual files, the size of which conform to the specified continuous probability-distribution; & then exit.",
			G.Option ""	["testPerformanceDiscrete"]	(
				testPerformanceDiscrete `G.ReqArg` "(<Int>, <DiscreteDistribution>)"
			)											"Measure the CPU-seconds (accumulated over all CPU-cores) required to find the best fit, for the specified number of randomly generated virtual files, the size of which conform to the specified discrete probability-distribution; & then exit."
		 ] where
			listDelimiters	= ('(', '|', ')')

			defaultMaximumBytes	= Data.CommandOptions.getMaximumBytes defaultCommandOptions

			setMaximumBytes, setMinimumUsageRatio, setVerbosity, testPerformanceContinuous, testPerformanceDiscrete :: String -> CommandLineAction
			setMaximumBytes arg commandOptions	= return {-to IO-monad-} commandOptions { Data.CommandOptions.getMaximumBytes = readCommandArg arg }
			setMinimumUsageRatio arg commandOptions	= return {-to IO-monad-} commandOptions { Data.CommandOptions.getMinimumUsageRatio = realToFrac (readCommandArg arg :: Double) }
			setVerbosity arg commandOptions		= return {-to IO-monad-} commandOptions { Data.CommandOptions.getVerbosity = readCommandArg arg }

			testPerformanceContinuous arg commandOptions
				| not $ ToolShed.SelfValidate.isValid commandOptions	= error $ ToolShed.SelfValidate.getFirstError commandOptions
				| otherwise						= do
					ToolShed.System.TimeAction.printCPUSeconds $ Test.Performance.run commandOptions fileCount probabilityDistribution >>= mapM_ print {-force evaluation-}

					System.Exit.exitSuccess
				where
					fileCount		:: Int
					probabilityDistribution	:: Factory.Math.Probability.ContinuousDistribution Double
					(fileCount, probabilityDistribution)	= readCommandArg arg

			testPerformanceDiscrete arg commandOptions
				| not $ ToolShed.SelfValidate.isValid commandOptions	= error $ ToolShed.SelfValidate.getFirstError commandOptions
				| otherwise						= do
					ToolShed.System.TimeAction.printCPUSeconds $ Test.Performance.run commandOptions fileCount probabilityDistribution >>= mapM_ print {-force evaluation-}

					System.Exit.exitSuccess
				where
					fileCount		:: Int
					probabilityDistribution	:: Factory.Math.Probability.DiscreteDistribution Double
					(fileCount, probabilityDistribution)	= readCommandArg arg

			setIncludeEmpty, setRandomSeed :: Maybe String -> CommandLineAction
			setIncludeEmpty arg commandOptions	= return {-to IO-monad-} commandOptions { Data.CommandOptions.getIncludeEmpty = Data.Maybe.maybe True readCommandArg arg }
			setRandomSeed arg commandOptions	= return {-to IO-monad-} commandOptions { Data.CommandOptions.getMaybeRandomSeed = Just $ Data.Maybe.maybe defaultRandomSeed readBoundedIntegral arg }

			printVersion, printUsage :: IO (Data.CommandOptions.CommandOptions f)
			printVersion	= System.IO.hPutStrLn System.IO.stderr (
				showString (Distribution.Text.display packageIdentifier) . showString "\nCompiled by " . shows compiler . showString ".\nWritten by " . shows author . showString ".\nCopyright (C) 2010-2015 " $ showString author ".\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it under certain conditions."
			 ) >> System.Exit.exitSuccess	where
				packageIdentifier :: Distribution.Package.PackageIdentifier
				packageIdentifier	= Distribution.Package.PackageIdentifier {
					Distribution.Package.pkgName	= Distribution.Package.PackageName progName,	-- CAVEAT: coincidentally.
					Distribution.Package.pkgVersion	= Distribution.Version.Version (Data.Version.versionBranch Paths.version) []
				}

				author, compiler :: String
				author		= "Dr. Alistair Ward"
				compiler	= System.Info.compilerName ++ "-" ++ Data.List.intercalate "." (map show $ Data.Version.versionBranch System.Info.compilerVersion)

			printUsage	= Text.Printf.hPrintf System.IO.stderr (
				showString "Usage:\t%s  [<File-path> ...]\n\nEBNF argument-format:" $ showString (
					concat $ replicate 9 "\n\t%-22s = %s;"	-- Argument-types & their EBNF-definition.
				) "\n\nE.g.\n\t%s\n\t%s\n\t%s\n"
			 ) (
				G.usageInfo progName optDescrList
			 ) "Bool" "\"True\" | \"False\"\t(* case-sensitive *)" "ContinuousDistribution" "LogNormalDistribution location scale^2" "DiscreteDistribution" "PoissonDistribution lambda" "File-path" (
				"File-name ('" ++ [System.FilePath.pathSeparator] ++ "' File-name)*"
			 ) "Float" "Int ('.' Int)?" "Int" "[0-9]+" "lambda" "Float\t(* the mean & variance of the distribution *)" "location" "Float\t(* the mean of the log of the distribution *)" "scale^2" "Float\t(* the variance of the log of the distribution *)" (
				progName ++ " --verbosity=Verbose -M 700000000 *.ogg +RTS -N\t#Find the best-fit for the globbed file-names, into the space available on a CD, using multiple CPU-cores where available."
			 ) (
				progName ++ " -r --testPerformanceContinuous='(100, LogNormalDistribution " ++ show (log ((fromIntegral defaultMaximumBytes / 12) / sqrt 2) :: Float) {-location-} ++ " " ++ show (log 2 :: Float) {-scale^2-} ++ ")'\t#Test performance."
			 ) (
				progName ++ " -r --testPerformanceDiscrete='(100, PoissonDistribution " ++ show (defaultMaximumBytes `div` 12) {-lambda-} ++ ")'\t#Test performance."
			 ) >> System.Exit.exitSuccess	-- CAVEAT: requires increase to default context-stack; see GHC-option at top of file.

	args	<- System.Environment.getArgs

--	G.getOpt :: G.ArgOrder CommandLineAction -> [G.OptDescr CommandLineAction] -> [String] -> ([CommandLineAction], [String], [String])
	case G.getOpt G.RequireOrder optDescrList args of
		(commandLineActions, nonOptions, [{-errors-}])	-> do
			commandOptions	<- Data.List.foldl' (>>=) (
				return {-to IO-monad-} Data.Default.def
			 ) {-transform using CommandLineAction-mutators-} commandLineActions	-- ie: do o1 <- CommandLineAction[0] commandOptions[0]; o2 <- CommandLineAction[1] o1; ...

			if not $ ToolShed.SelfValidate.isValid commandOptions
				then error $ ToolShed.SelfValidate.getFirstError commandOptions
				else if null nonOptions
					then error "zero file-paths specified."
					else let
						standardInputProxy	= "-"
					in do
						filePaths	<- Data.List.nub {-remove explicit duplicates-} <$> if standardInputProxy `elem` nonOptions
							then let
								getFilePaths :: IO Data.File.FilePathList
								getFilePaths	= do
									eof	<- System.IO.isEOF

									if eof
										then return {-to IO-monad-} []
										else {-more to read-} (:) <$> getLine <*> getFilePaths {-recurse-}
							in do
								filePaths	<- (filter (/= standardInputProxy) nonOptions ++) <$> getFilePaths

								if null filePaths
									then error "zero file-paths."
									else return filePaths
							else {-real fileNames-} return {-to IO-monad-} nonOptions

						implicitDuplicateFilePaths	<- Data.File.findDuplicates filePaths

						Control.Monad.unless (Data.CommandOptions.getVerbosity commandOptions == minBound || null implicitDuplicateFilePaths) . System.IO.hPutStrLn System.IO.stderr . showString "WARNING: there are duplicate files implicit within those directories specified; " $ shows implicitDuplicateFilePaths "."

						(acceptedFileSizeAndPathList, logFile)	<- Control.Monad.Writer.runWriter . Data.File.selectSuitableFileSizes (<= Data.CommandOptions.getMaximumBytes commandOptions) <$> Data.File.findSizes filePaths

						Control.Monad.unless (Data.CommandOptions.getVerbosity commandOptions == minBound || null logFile) . System.IO.hPutStrLn System.IO.stderr $ Data.List.intercalate "\n" logFile

						if null acceptedFileSizeAndPathList
							then error "there are zero suitable files."
							else let
								aggregateSize	= Data.File.aggregateSize acceptedFileSizeAndPathList
								minimumBytes	= Data.CommandOptions.deriveMinimumBytes commandOptions
							in if aggregateSize < minimumBytes
								then error . showString "the aggregate size of all suitable files (" . shows aggregateSize . showString " bytes), is insufficient to satisfy the minimum " $ shows minimumBytes " bytes required."
								else Squeeze.partitionEmptyFilesAndDistributeAndFindBestFit commandOptions acceptedFileSizeAndPathList >>= mapM_ print {-lazy evaluation-}
		(_, _, errors)	-> System.IO.Error.ioError . System.IO.Error.userError $ concatMap init {-chop-} errors

