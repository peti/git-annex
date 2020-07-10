{- git ref stuff
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Ref where

import Common
import Git
import Git.Command
import Git.Sha
import Git.Types
import Git.FilePath

import Data.Char (chr, ord)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

headRef :: Ref
headRef = Ref "HEAD"

headFile :: Repo -> FilePath
headFile r = fromRawFilePath (localGitDir r) </> "HEAD"

setHeadRef :: Ref -> Repo -> IO ()
setHeadRef ref r = S.writeFile (headFile r) ("ref: " <> fromRef' ref)

{- Converts a fully qualified git ref into a user-visible string. -}
describe :: Ref -> String
describe = fromRef . base

{- Often git refs are fully qualified 
 - (eg refs/heads/master or refs/remotes/origin/master).
 - Converts such a fully qualified ref into a base ref
 - (eg: master or origin/master). -}
base :: Ref -> Ref
base = removeBase "refs/heads/" . removeBase "refs/remotes/"

{- Removes a directory such as "refs/heads/master" from a
 - fully qualified ref. Any ref not starting with it is left as-is. -}
removeBase :: String -> Ref -> Ref
removeBase dir r
	| prefix `isPrefixOf` rs = Ref $ encodeBS $ drop (length prefix) rs
	| otherwise = r
  where
	rs = fromRef r
	prefix = case end dir of
		['/'] -> dir
		_ -> dir ++ "/"

{- Given a directory such as "refs/remotes/origin", and a ref such as
 - refs/heads/master, yields a version of that ref under the directory,
 - such as refs/remotes/origin/master. -}
underBase :: String -> Ref -> Ref
underBase dir r = Ref $ encodeBS dir <> "/" <> fromRef' (base r)

{- Convert a branch such as "master" into a fully qualified ref. -}
branchRef :: Branch -> Ref
branchRef = underBase "refs/heads"

{- A Ref that can be used to refer to a file in the repository, as staged
 - in the index.
 -
 - Prefixing the file with ./ makes this work even if in a subdirectory
 - of a repo.
 -}
fileRef :: RawFilePath -> Ref
fileRef f = Ref $ ":./" <> toInternalGitPath f

{- A Ref that can be used to refer to a file in a particular branch. -}
branchFileRef :: Branch -> RawFilePath -> Ref
branchFileRef branch f = Ref $ fromRef' branch <> ":" <> toInternalGitPath f

{- Converts a Ref to refer to the content of the Ref on a given date. -}
dateRef :: Ref -> RefDate -> Ref
dateRef r (RefDate d) = Ref $ fromRef' r <> "@" <> encodeBS' d

{- A Ref that can be used to refer to a file in the repository as it
 - appears in a given Ref. -}
fileFromRef :: Ref -> RawFilePath -> Ref
fileFromRef r f = let (Ref fr) = fileRef f in Ref (fromRef' r <> fr)

{- Checks if a ref exists. -}
exists :: Ref -> Repo -> IO Bool
exists ref = runBool
	[ Param "show-ref"
	, Param "--verify"
	, Param "-q"
	, Param $ fromRef ref
	]

{- The file used to record a ref. (Git also stores some refs in a
 - packed-refs file.) -}
file :: Ref -> Repo -> FilePath
file ref repo = fromRawFilePath (localGitDir repo) </> fromRef ref

{- Checks if HEAD exists. It generally will, except for in a repository
 - that was just created. -}
headExists :: Repo -> IO Bool
headExists repo = do
	ls <- S.split nl <$> pipeReadStrict [Param "show-ref", Param "--head"] repo
	return $ any (" HEAD" `S.isSuffixOf`) ls
  where
	nl = fromIntegral (ord '\n')

{- Get the sha of a fully qualified git ref, if it exists. -}
sha :: Branch -> Repo -> IO (Maybe Sha)
sha branch repo = process <$> showref repo
  where
	showref = pipeReadStrict
		[ Param "show-ref"
		, Param "--hash" -- get the hash
		, Param $ fromRef branch
		]
	process s
		| S.null s = Nothing
		| otherwise = Just $ Ref $ firstLine' s

headSha :: Repo -> IO (Maybe Sha)
headSha = sha headRef

{- List of (shas, branches) matching a given ref or refs. -}
matching :: [Ref] -> Repo -> IO [(Sha, Branch)]
matching = matching' []

{- Includes HEAD in the output, if asked for it. -}
matchingWithHEAD :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingWithHEAD = matching' [Param "--head"]

matching' :: [CommandParam] -> [Ref] -> Repo -> IO [(Sha, Branch)]
matching' ps rs repo = map gen . S8.lines <$> 
	pipeReadStrict (Param "show-ref" : ps ++ rps) repo
  where
	gen l = let (r, b) = separate' (== fromIntegral (ord ' ')) l
		in (Ref r, Ref b)
	rps = map (Param . fromRef) rs

{- List of (shas, branches) matching a given ref.
 - Duplicate shas are filtered out. -}
matchingUniq :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingUniq refs repo = nubBy uniqref <$> matching refs repo
  where
	uniqref (a, _) (b, _) = a == b

{- List of all refs. -}
list :: Repo -> IO [(Sha, Ref)]
list = matching' [] []

{- Deletes a ref. This can delete refs that are not branches, 
 - which git branch --delete refuses to delete. -}
delete :: Sha -> Ref -> Repo -> IO ()
delete oldvalue ref = run
	[ Param "update-ref"
	, Param "-d"
	, Param $ fromRef ref
	, Param $ fromRef oldvalue
	]

{- Gets the sha of the tree a ref uses. 
 -
 - The ref may be something like a branch name, and it could contain
 - ":subdir" if a subtree is wanted. -}
tree :: Ref -> Repo -> IO (Maybe Sha)
tree (Ref ref) = extractSha <$$> pipeReadStrict
	[ Param "rev-parse"
	, Param "--verify"
	, Param "--quiet"
	, Param (decodeBS' ref')
	]
  where
	ref' = if ":" `S.isInfixOf` ref
		then ref
		-- de-reference commit objects to the tree
		else ref <> ":"

{- Checks if a String is a legal git ref name.
 -
 - The rules for this are complex; see git-check-ref-format(1) -}
legal :: Bool -> String -> Bool
legal allowonelevel s = all (== False) illegal
  where
	illegal =
		[ any ("." `isPrefixOf`) pathbits
		, any (".lock" `isSuffixOf`) pathbits
		, not allowonelevel && length pathbits < 2
		, contains ".."
		, any (\c -> contains [c]) illegalchars
		, begins "/"
		, ends "/"
		, contains "//"
		, ends "."
		, contains "@{"
		, null s
		]
	contains v = v `isInfixOf` s
	ends v = v `isSuffixOf` s
	begins v = v `isPrefixOf` s

	pathbits = splitc '/' s
	illegalchars = " ~^:?*[\\" ++ controlchars
	controlchars = chr 0o177 : [chr 0 .. chr (0o40-1)]
