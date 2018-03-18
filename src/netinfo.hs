{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, ExistentialQuantification #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

--import Control.Monad
import System.Console.ANSI (clearScreen) --TODO add some colors :)
import System.Environment (getArgs)

import System.Linux.NetInfo
import System.Linux.NetInfo.Probe

--https://serverfault.com/questions/648140/how-to-scan-ipv6-enabled-hosts-on-my-lan
--https://superuser.com/questions/1135757/scanning-in-ipv6

--------------------------------------------------------------------------------
#if 0
putTrace :: Show a => TQueue Trace -> a -> IO ()
putTrace q w = atomically $ writeTQueue q (Trace w)

data Trace = forall a. Show a => Trace a

startTraceThread :: IO (TQueue Trace)
startTraceThread
	= newTQueueIO
	>>= \q -> (void $ forkIO $ forever $ atomically (readTQueue q)
		>>= \(Trace a) -> print a)
	>> return q
#endif
--------------------------------------------------------------------------------

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

