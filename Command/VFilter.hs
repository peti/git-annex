{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.VFilter where

import Command
import Annex.View
import Command.View (paramView, checkoutViewBranch)

cmd :: Command
cmd = notBareRepo $
	command "vfilter" SectionMetaData "filter current view"
		paramView (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	showStart "vfilter" ""
	withCurrentView $ \view -> do
		let view' = filterView view $
			map parseViewParam $ reverse params
		next $ next $ if visibleViewSize view' > visibleViewSize view
			then error "That would add an additional level of directory structure to the view, rather than filtering it. If you want to do that, use vadd instead of vfilter."
			else checkoutViewBranch view' narrowView
