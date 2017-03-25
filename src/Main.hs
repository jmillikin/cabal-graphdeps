-- |
-- Module: Main
-- License: MIT
--
-- Run like:
-- >$ cabal-graphdeps <mypackage> | tred | dot -Tpng > ~/<mypackage>.png
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad (foldM, forM_, when)
import           Data.Char (isSpace)
import           Data.List (isInfixOf, isSuffixOf, stripPrefix)
import           Data.List.Split (splitWhen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Options
import qualified System.Directory as Filesystem
import           System.Exit (exitFailure)
import qualified System.Process as Process
import qualified System.IO as IO
import           System.IO.Temp (withSystemTempDirectory)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

data MainOptions = MainOptions
	{ optGlobalPackageDb :: String
	, optGlobalPackages :: [String]
	, optExclude :: Set.Set String
	, optCabal :: String
	, optGhcPkg :: String
	, optAddSources :: [FilePath]
	, optCabalConfig :: Maybe FilePath
	, optUseAllGlobalPackages :: Bool
	}

instance Options MainOptions where
	defineOptions = pure MainOptions
		<*> simpleOption "global-package-db" ""
		    ""
		<*> defineOption (optionType_list ',' optionType_string) (\o -> o
			{ optionLongFlags = ["global-packages"]
			, optionDefault = ["base"]
			, optionDescription = "These comma-separated packages and their dependencies will be taken from the global package db. See also '--use-all-global-packages'."
			})
		<*> defineOption (optionType_set ',' optionType_string) (\o -> o
			{ optionLongFlags = ["exclude-packages"]
			, optionDefault = Set.empty
			, optionDescription = "These comma-separated packages and their dependencies will be excluded when rendering the graph."
			})
		<*> defineOption optionType_string (\o -> o
			{ optionLongFlags = ["cabal"]
			, optionDefault = "cabal"
			, optionDescription = "The name of or path to the cabal executable."
			})
		<*> defineOption optionType_string (\o -> o
			{ optionLongFlags = ["ghc-pkg"]
			, optionDefault = "ghc-pkg"
			, optionDescription = "The name of or path to the ghc-pkg executable."
			})
		<*> defineOption (optionType_list ',' optionType_string) (\o -> o
			{ optionLongFlags = ["add-sources"]
			, optionDefault = []
			, optionDescription = "These comma-separated paths to local packages will be added to the Cabal sandbox with 'cabal sandbox add-source'."
			})
		<*> defineOption (optionType_maybe optionType_string) (\o -> o
			{ optionLongFlags = ["cabal-config"]
			, optionDefault = Nothing
			, optionDescription = "The path to a 'cabal.config' file, e.g. to specify constraints."
			})
		<*> defineOption (optionType_bool) (\o -> o
			{ optionLongFlags = ["use-all-global-packages"]
			, optionDefault = False
			, optionDescription = "If this option is true then all packages in the global package db will be used. When this option is true the '--global-packages' option is ignored, since those packages must be a subset of the packages available in the global package db; this option is the same as listed all globally installed packages as arguments to '--global-packages'."
			})

resolveDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> IO (Map.Map String (Set.Set String))
resolveDeps _ allDeps pkg | Map.member pkg allDeps = return allDeps
resolveDeps opts allDeps pkg | Set.member (extractPkgName pkg) (optExclude opts) = return allDeps
resolveDeps opts allDeps pkg = do
	let header1 = "Resolving dependencies...\nIn order, the following would be installed (use -v for more details):\n"
	let header2 = "Resolving dependencies...\nIn order, the following would be installed:\n"
	
	IO.hPutStrLn IO.stderr ("[" ++ pkg ++ "]")
	output <- Process.readProcess (optCabal opts) ["--package-db=clear", "install", "--dry-run", "--ghc-pkg-options=--global-package-db=./empty-db", pkg] ""
	if "All the requested packages are already installed:" `isInfixOf` output
		then return allDeps
		else do
			trimmedOutput <- case stripPrefix header1 output of
				Just stripped -> return stripped
				Nothing -> case stripPrefix header2 output of
					Just stripped -> return stripped
					Nothing -> do
						IO.hPutStrLn IO.stderr (show output)
						error "unknown output format"
			let newDeps = linesToDeps pkg (splitWhen (== '\n') trimmedOutput)
			let insertedDeps = Map.insertWith Set.union pkg newDeps allDeps
			foldM (resolveDeps opts) insertedDeps (Set.toList newDeps)

linesToDeps :: String -> [String] -> Set.Set String
linesToDeps pkg lines = Set.fromList $ do
	line <- filter (not . null) lines
	parsedLine <- case stringMatch lineOnlyVersion line of
		Just _ -> return line
		Nothing -> case stringMatch lineVersionWithNote line of
			Just version -> return version
			Nothing -> error ("can't parse line " ++ show line)
	if parsedLine == pkg
		then []
		else [parsedLine]

stringMatch :: Parsec.Parser a -> String -> Maybe a
stringMatch parser input = case Parsec.parse parser "" input of
	Left _ -> Nothing
	Right x -> Just x

lineOnlyVersion :: Parsec.Parser ()
lineOnlyVersion = do
	Parsec.skipMany1 alphaNumDot
	Parsec.eof

lineVersionWithNote :: Parsec.Parser String
lineVersionWithNote = do
	version <- Parsec.many1 alphaNumDot
	_ <- Parsec.char ' '
	_ <- Parsec.char '('
	Parsec.skipMany1 Parsec.anyChar
	Parsec.eof
	return version

alphaNumDot :: Parsec.Parser Char
alphaNumDot = Parsec.lower <|> Parsec.upper <|> Parsec.digit <|> Parsec.oneOf "-."

renderDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> Set.Set String
renderDeps opts deps rootPkg = rendered where
	(_, _, rendered) = loop rootPkg (rootPkg, Set.empty, Set.empty)
	
	-- This package has already been visited, so we don't need to continue
	-- any further.
	loop pkg acc@(_, visited, _) | Set.member pkg visited = acc
	
	-- map "foo-bar-baz-1.0" to "foo-bar-baz" for excluded packages
	loop pkg acc | Set.member (extractPkgName pkg) (optExclude opts) = acc
	
	loop pkg (parent, visited, lines) = let
		pkgDeps = Set.toList (Map.findWithDefault Set.empty pkg deps)
		visited' = Set.insert pkg visited
		lines' = Set.union lines $ Set.fromList $ do
			dep <- pkgDeps
			-- map "foo-bar-baz-1.0" to "foo-bar-baz" for excluded packages
			if Set.member (extractPkgName dep) (optExclude opts)
				then []
				else [show pkg ++ " -> " ++ show dep]
		(_, visited'', lines'') = foldr loop (pkg, visited', lines') pkgDeps
		in (parent, visited'', lines'')

extractPkgName :: String -> String
extractPkgName pkg = case stripPrefix "-" (dropWhile (/= '-') (reverse pkg)) of
	Nothing -> pkg
	Just rev -> reverse rev

printDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> IO ()
printDeps opts deps pkg = forM_ (Set.toAscList (renderDeps opts deps pkg)) putStrLn

readGhcPkgField :: MainOptions -> String -> String -> IO String
readGhcPkgField opts pkgName fieldName = do
	rawField <- Process.readProcess (optGhcPkg opts) ["field", pkgName, fieldName] ""
	case stripPrefix (fieldName ++ ":") rawField of
		Nothing -> error ("Unexpected output from ghc-pkg field: " ++ show rawField)
		Just s -> return (dropWhile isSpace (dropWhileEnd isSpace s))

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

initSandbox :: MainOptions -> IO ()
initSandbox opts = do
	_ <- Process.readProcess (optCabal opts) ["sandbox", "init"] ""
	forM_ (optAddSources opts) $ \path ->
		Process.readProcess (optCabal opts) ["sandbox", "add-source", path] ""
	Filesystem.createDirectory "empty-db"
	
	globalDb <- case optGlobalPackageDb opts of
		-- Look for the global package DB
		"" -> do
			rawGlobalDb <- Process.readProcess (optGhcPkg opts) ["list", "--no-user-package-db"] ""
			case lines rawGlobalDb of
				firstLine:_ ->
					-- Some versions of ghc-pkg adds a ':' to the first line when not
					-- run from a terminal
					if last firstLine == ':'
					then return (init firstLine)
					else return firstLine
				_ -> error "Unexpected output from ghc-pkg list"
		path -> return path

	-- All globally installed packages will be assumed installed at
	-- their globally installed versions. They will be excluded from the
	-- dependency graph.
	when (optUseAllGlobalPackages opts) $ do
		globalDbContents <- Filesystem.listDirectory globalDb
		forM_ globalDbContents $ \file -> do
			when (".conf" `isSuffixOf` file) $
				Filesystem.copyFile (globalDb ++ "/" ++ file) ("empty-db/" ++ file)

	-- these packages and their dependencies will be excluded from the
	-- graph.
	when (not $ optUseAllGlobalPackages opts) $ forM_ (optGlobalPackages opts) $ \pkg -> do
		-- find package id (used in .conf filename)
		pkgId <- readGhcPkgField opts pkg "id"
		let pkgConf = pkgId ++ ".conf"
		
		-- try to figure out what the package depends on; in the GHC
		-- installed on my system, ::depends includes all dependencies
		-- of the package.
		deps <- readGhcPkgField opts pkg "depends"
		let splitDeps = words deps
		let emptyPackageDbConfs = pkgConf : [s ++ ".conf" | s <- splitDeps]
		forM_ emptyPackageDbConfs $ \conf -> Filesystem.copyFile (globalDb ++ "/" ++ conf) ("empty-db/" ++ conf)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir io = bracket
	(do
		cwd <- Filesystem.getCurrentDirectory
		Filesystem.setCurrentDirectory dir
		return cwd)
	Filesystem.setCurrentDirectory
	(\_ -> io)

main :: IO ()
main = runCommand $ \opts args -> do
	rootPackageName <- case args of
		[x] -> return x
		_ -> do
			IO.hPutStrLn IO.stderr "Usage: cabal-graphdeps <package-name>"
			exitFailure
	-- Make all paths in the options absolute because we're about to cd
	-- into a temp dir.
	opts <- do
		let addSources = optAddSources opts
		let cabalConfig = optCabalConfig opts
		absoluteAddSources <- mapM Filesystem.makeAbsolute addSources
		absoluteCabalConfig <- mapM Filesystem.makeAbsolute cabalConfig
		return (opts { optAddSources = absoluteAddSources, optCabalConfig = absoluteCabalConfig })
	deps <- withSystemTempDirectory "cabal-graphdeps.d-" $ \dir -> withCurrentDirectory dir $ do
		initSandbox opts
		case optCabalConfig opts of
			Nothing -> return ()
			Just path -> Filesystem.copyFile path "cabal.config"
		resolveDeps opts Map.empty rootPackageName
	putStrLn "digraph {"
	printDeps opts deps rootPackageName
	putStrLn "}"
