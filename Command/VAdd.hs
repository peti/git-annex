{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VAdd where

import Command
import Annex.View
import Command.View (checkoutViewBranch)

cmd :: Command
cmd = notBareRepo $
	command "vadd" SectionMetaData 
		"add subdirs to current view"
		(paramRepeating "FIELD=GLOB")
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	showStart "vadd" ""
	withCurrentView $ \view -> do
		let (view', change) = refineView view $
			map parseViewParam $ reverse params
		case change of
			Unchanged -> do
				showNote "unchanged"
				next $ next $ return True
			Narrowing -> next $ next $ do
				if visibleViewSize view' == visibleViewSize view
					then error "That would not add an additional level of directory structure to the view. To filter the view, use vfilter instead of vadd."
					else checkoutViewBranch view' narrowView
			Widening -> error "Widening view to match more files is not currently supported."
