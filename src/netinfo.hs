{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP, ExistentialQuantification #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import Data.Word
import Network.Socket hiding (sendTo, recvFrom)
import System.Console.ANSI (clearScreen) --TODO add some colors :)
import System.Environment (getArgs)

import System.Linux.NetInfo
import System.Linux.Ping

--https://serverfault.com/questions/648140/how-to-scan-ipv6-enabled-hosts-on-my-lan
--https://superuser.com/questions/1135757/scanning-in-ipv6

--------------------------------------------------------------------------------

-- | Ping all IPs in subnet
pingSubnet :: (String -> IO ()) -> Socket -> IP -> Word8 -> IO ()
pingSubnet trace sock ipaddr@(IPv4 host) mask = do
	trace $ show (here, "Begin", mask, ipaddr)
	let pingList = ipv4SubnetPingList host mask --FIXME FIXME check if subnet is not already being scanned
	forM_ pingList $ \h -> do
		sendPing sock h
		trace $ show (here, IPv4 h)
		--threadDelay 1000
	trace $ show (here, "Done", ipaddr, length pingList)
pingSubnet trace _ ip mask = trace $ show (here, ip, mask, "IGNORED!")

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

startPingThread :: IO (TQueue (IP, Word8))
startPingThread = do
	pingQ <- newTQueueIO

	let qputstr _ = return ()

	forkIO $ withPingSocket Nothing $ \sock -> do
#if 1
-- FIXME do not let sock leave the bracket
		forkIO $ forever $ do
	--		atomically $ modifyTVar pingPending $ \case
	--			0 -> 0
	--			x -> x - 1
			(msg, src) <- receivePingFrom sock
			case msg of
				Right IcmpHeader{icmp_type = 0, code = 0} -> do
					qputstr $ show (here, "ping response from", src)
				_ -> return ()
#endif
		forever $ do
--FIXME keep some state, timeouts, do not DOS something
			(ip, netmask) <- atomically $ readTQueue pingQ
--			atomically $ modifyTVar pingPending (+1)
--			atomically $ readTVar pingPending >>= (guard . ( <= 8))
			pingSubnet qputstr sock ip netmask

	return pingQ

--------------------------------------------------------------------------------

{-
fping -ag 192.168.0.0/24
-}

main :: IO ()
main = do
#if 1
	args <- getArgs
	let active = take 1 args == ["--active"]

	eventHandler <- if active
		then do
			pingQ <- startPingThread
			return $ \event -> case event of
				AddSubnet _ ip (IfNet mask)
					-> atomically $ writeTQueue pingQ (ip, mask)
				DelSubnet _ ip (IfNet mask)
					-> print here --error here --TODO remove from ping thread thread rotation
				_ -> return ()
		else
			return (const (return ()))
#else
	let eventHandler = const (return ())
#endif

	withNetInfo $ flip newsLoop $
		\(event, ifMap) -> do

			maybe (return ()) eventHandler event

			clearScreen
			putStrLn "--------------------------------------------------------------------------------"
			dumpIfMap putStrLn ifMap
			return Nothing

