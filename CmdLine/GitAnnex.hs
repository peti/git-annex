{- git-annex main program
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, OverloadedStrings #-}

module CmdLine.GitAnnex where

import qualified Git.CurrentRepo
import CmdLine
import Command
import Utility.Env
import Annex.Ssh
import Types.Test

import qualified Command.Help
import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.Fsck
import qualified Command.LookupKey
import qualified Command.ContentLocation
import qualified Command.ExamineKey
import qualified Command.MatchExpression
import qualified Command.FromKey
import qualified Command.RegisterUrl
import qualified Command.SetKey
import qualified Command.DropKey
import qualified Command.TransferKey
import qualified Command.TransferKeys
import qualified Command.SetPresentKey
import qualified Command.ReadPresentKey
import qualified Command.CheckPresentKey
import qualified Command.ReKey
import qualified Command.MetaData
import qualified Command.View
import qualified Command.VAdd
import qualified Command.VFilter
import qualified Command.VPop
import qualified Command.VCycle
import qualified Command.Reinject
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Describe
import qualified Command.InitRemote
import qualified Command.EnableRemote
import qualified Command.Expire
import qualified Command.Repair
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.AddUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.FindRef
import qualified Command.Whereis
import qualified Command.List
import qualified Command.Log
import qualified Command.Merge
import qualified Command.ResolveMerge
import qualified Command.Info
import qualified Command.Status
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Reinit
import qualified Command.NumCopies
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.Dead
import qualified Command.Group
import qualified Command.Wanted
import qualified Command.GroupWanted
import qualified Command.Required
import qualified Command.Schedule
import qualified Command.Ungroup
import qualified Command.Vicfg
import qualified Command.Sync
import qualified Command.Mirror
import qualified Command.AddUrl
import qualified Command.ImportFeed
import qualified Command.RmUrl
import qualified Command.Import
import qualified Command.Map
import qualified Command.Upgrade
import qualified Command.Forget
import qualified Command.DiffDriver
import qualified Command.Smudge
import qualified Command.Undo
import qualified Command.Version
#ifdef WITH_ASSISTANT
import qualified Command.Watch
import qualified Command.Assistant
#ifdef WITH_WEBAPP
import qualified Command.WebApp
#endif
#ifdef WITH_XMPP
import qualified Command.XMPPGit
#endif
import qualified Command.RemoteDaemon
#endif
import qualified Command.Test
#ifdef WITH_TESTSUITE
import qualified Command.FuzzTest
import qualified Command.TestRemote
#endif
#ifdef WITH_BENCHMARK
import qualified Command.Benchmark
#endif
#ifdef WITH_EKG
import System.Remote.Monitoring
#endif

cmds :: Parser TestOptions -> Maybe TestRunner -> [Command]
cmds testoptparser testrunner = 
	[ Command.Help.cmd
	, Command.Add.cmd
	, Command.Get.cmd
	, Command.Drop.cmd
	, Command.Move.cmd
	, Command.Copy.cmd
	, Command.Fsck.cmd
	, Command.Unlock.cmd
	, Command.Unlock.editcmd
	, Command.Lock.cmd
	, Command.Sync.cmd
	, Command.Mirror.cmd
	, Command.AddUrl.cmd
	, Command.ImportFeed.cmd
	, Command.RmUrl.cmd
	, Command.Import.cmd
	, Command.Init.cmd
	, Command.Describe.cmd
	, Command.InitRemote.cmd
	, Command.EnableRemote.cmd
	, Command.Reinject.cmd
	, Command.Unannex.cmd
	, Command.Uninit.cmd
	, Command.Reinit.cmd
	, Command.PreCommit.cmd
	, Command.NumCopies.cmd
	, Command.Trust.cmd
	, Command.Untrust.cmd
	, Command.Semitrust.cmd
	, Command.Dead.cmd
	, Command.Group.cmd
	, Command.Wanted.cmd
	, Command.GroupWanted.cmd
	, Command.Required.cmd
	, Command.Schedule.cmd
	, Command.Ungroup.cmd
	, Command.Vicfg.cmd
	, Command.LookupKey.cmd
	, Command.ContentLocation.cmd
	, Command.ExamineKey.cmd
	, Command.MatchExpression.cmd
	, Command.FromKey.cmd
	, Command.RegisterUrl.cmd
	, Command.SetKey.cmd
	, Command.DropKey.cmd
	, Command.TransferKey.cmd
	, Command.TransferKeys.cmd
	, Command.SetPresentKey.cmd
	, Command.ReadPresentKey.cmd
	, Command.CheckPresentKey.cmd
	, Command.ReKey.cmd
	, Command.MetaData.cmd
	, Command.View.cmd
	, Command.VAdd.cmd
	, Command.VFilter.cmd
	, Command.VPop.cmd
	, Command.VCycle.cmd
	, Command.Fix.cmd
	, Command.Expire.cmd
	, Command.Repair.cmd
	, Command.Unused.cmd
	, Command.DropUnused.cmd
	, Command.AddUnused.cmd
	, Command.Find.cmd
	, Command.FindRef.cmd
	, Command.Whereis.cmd
	, Command.List.cmd
	, Command.Log.cmd
	, Command.Merge.cmd
	, Command.ResolveMerge.cmd
	, Command.Info.cmd
	, Command.Status.cmd
	, Command.Migrate.cmd
	, Command.Map.cmd
	, Command.Upgrade.cmd
	, Command.Forget.cmd
	, Command.DiffDriver.cmd
	, Command.Smudge.cmd
	, Command.Undo.cmd
	, Command.Version.cmd
#ifdef WITH_ASSISTANT
	, Command.Watch.cmd
	, Command.Assistant.cmd
#ifdef WITH_WEBAPP
	, Command.WebApp.cmd
#endif
#ifdef WITH_XMPP
	, Command.XMPPGit.cmd
#endif
	, Command.RemoteDaemon.cmd
#endif
	, Command.Test.cmd testoptparser testrunner
#ifdef WITH_TESTSUITE
	, Command.FuzzTest.cmd
	, Command.TestRemote.cmd
#endif
#ifdef WITH_BENCHMARK
	, Command.Benchmark.cmd
#endif
	]

run :: Parser TestOptions -> Maybe TestRunner -> [String] -> IO ()
run testoptparser testrunner args = do
#ifdef WITH_EKG
	_ <- forkServer "localhost" 4242
#endif
	go envmodes
  where
	go [] = dispatch True args 
		(cmds testoptparser testrunner)
		gitAnnexGlobalOptions [] Git.CurrentRepo.get
		"git-annex"
		"manage files with git, without checking their contents in"
	go ((v, a):rest) = maybe (go rest) a =<< getEnv v
	envmodes =
		[ (sshOptionsEnv, runSshOptions args)
		, (sshAskPassEnv, runSshAskPass)
		]
