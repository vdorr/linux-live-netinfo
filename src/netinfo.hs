{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, ExistentialQuantification #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

--import Control.Monad
import System.Console.ANSI (clearScreen) --TODO add some colors :)
import System.Environment (getArgs)

import System.Linux.NetInfo
import System.Linux.NetInfo.Probe

--FIXME handle ctrl+c cleanly
main :: IO ()
main = do
	args <- getArgs
	let active = take 1 args == ["--active"]

	let pingLayer
		| active = \nis body -> withPingNewSubnets nis $
			\ps ->
				pingAll ps --ping everything once on start
				>> body
		| otherwise = \_ body -> body

	withNetInfo $ \netInfoSock -> do
		pingLayer netInfoSock $  do
			newsLoop netInfoSock $ \(event, ifMap) -> do
				clearScreen
				putStrLn "--------------------------------------------------------------------------------"
				dumpIfMap putStrLn ifMap
				return Nothing

