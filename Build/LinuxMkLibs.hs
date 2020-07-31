{- Linux library copier and binary shimmer
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Main where

import System.Environment
import Data.Maybe
import System.FilePath
import Control.Monad
import Data.List
import System.Posix.Files
import Control.Monad.IfElse
import Control.Applicative
import Prelude

import Utility.LinuxMkLibs
import Utility.Directory
import Utility.Process
import Utility.Monad
import Utility.Path
import Utility.FileMode
import Utility.CopyFile

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify LINUXSTANDALONE_DIST"
	go (top:_) = mklibs top

mklibs :: FilePath -> IO ()
mklibs top = do
	fs <- dirContentsRecursive top
	exes <- filterM checkExe fs
	libs <- parseLdd <$> readProcess "ldd" exes
	
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	let (linkers, otherlibs) = partition ("ld-linux" `isInfixOf`) libs'
	libdirs <- nub . catMaybes <$> mapM (installLib installFile top) otherlibs
	libdirs' <- consolidateUsrLib top libdirs

	gconvlibs <- gconvLibs
	mapM_ (installLib installFile top) gconvlibs

	-- Various files used by runshell to set up env vars used by the
	-- linker shims.
	writeFile (top </> "libdirs") (unlines libdirs')
	writeFile (top </> "gconvdir") (parentDir $ Prelude.head gconvlibs)
	
	mapM_ (installLib installFile top) linkers
	let linker = Prelude.head linkers
	mapM_ (installLinkerShim top linker) exes
	
{- If there are two libdirs that are the same except one is in
 - usr/lib and the other is in lib/, move the contents of the usr/lib one
 - into the lib/ one. This reduces the number of directories the linker
 - needs to look in, and so reduces the number of failed stats
 - and improves startup time.
 -}
consolidateUsrLib :: FilePath -> [FilePath] -> IO [FilePath]
consolidateUsrLib top libdirs = map reverse <$> go [] (map reverse libdirs)
  where
	go c [] = return c
	go c (x:[]) = return (x:c)
	go c (x:y:rest)
		| x == y ++ reverse ("/usr") = do
			let x' = reverse x
			let y' = reverse y
			fs <- getDirectoryContents (inTop top x')
			forM_ fs $ \f -> do
				print f
				unless (dirCruft f) $
					renameFile 
						(inTop top (x' </> f))
						(inTop top (y' </> f))
			go (y:c) rest
		| otherwise = do
			print (x,y)
			go (x:c) (y:rest)

{- Installs a linker shim script around a binary.
 -
 - Note that each binary is put into its own separate directory,
 - to avoid eg git looking for binaries in its directory rather
 - than in PATH.
 -
 - The linker is symlinked to a file with the same basename as the binary,
 - since that looks better in ps than "ld-linux.so".
 -}
installLinkerShim :: FilePath -> FilePath -> FilePath -> IO ()
installLinkerShim top linker exe = do
	createDirectoryIfMissing True (top </> shimdir)
	createDirectoryIfMissing True (top </> exedir)
	ifM (isSymbolicLink <$> getSymbolicLinkStatus exe)
		( do
			sl <- readSymbolicLink exe
			nukeFile exe
			nukeFile exedest
			-- Assume that for a symlink, the destination
			-- will also be shimmed.
			let sl' = ".." </> takeFileName sl </> takeFileName sl
			createSymbolicLink sl' exedest
		, renameFile exe exedest
		)
	link <- relPathDirToFile (top </> exedir) (top ++ linker)
	unlessM (doesFileExist (top </> exelink)) $
		createSymbolicLink link (top </> exelink)
	writeFile exe $ unlines
		[ "#!/bin/sh"
		, "GIT_ANNEX_PROGRAMPATH=\"$0\""
		, "export GIT_ANNEX_PROGRAMPATH"
		, "exec \"$GIT_ANNEX_DIR/" ++ exelink ++ "\" --library-path \"$GIT_ANNEX_LD_LIBRARY_PATH\" \"$GIT_ANNEX_DIR/shimmed/" ++ base ++ "/" ++ base ++ "\" \"$@\""
		]
	modifyFileMode exe $ addModes executeModes
  where
	base = takeFileName exe
	shimdir = "shimmed" </> base
	exedir = "exe"
	exedest = top </> shimdir </> base
	exelink = exedir </> base

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	void $ copyFileExternal CopyTimeStamps f destdir
  where
	destdir = inTop top $ parentDir f

checkExe :: FilePath -> IO Bool
checkExe f
	| ".so" `isSuffixOf` f = return False
	| otherwise = ifM (isExecutable . fileMode <$> getFileStatus f)
		( checkFileExe <$> readProcess "file" ["-L", f]
		, return False
		)

{- Check that file(1) thinks it's a Linux ELF executable, or possibly
 - a shared library (a few executables like ssh appear as shared libraries). -}
checkFileExe :: String -> Bool
checkFileExe s = and
	[ "ELF" `isInfixOf` s
	, "executable" `isInfixOf` s || "shared object" `isInfixOf` s
	]
