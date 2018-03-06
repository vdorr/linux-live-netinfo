{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module System.Linux.NetInfo.Probe (
	  PingSocket
	, pingSubnet
	, withPingNewSubnets
) where

import System.Linux.NetInfo
import System.Linux.Ping

import Network.Socket
import Data.Word
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

--------------------------------------------------------------------------------

-- | Ping all IPs in subnet
pingSubnet :: (String -> IO ()) -> Socket -> IP -> Word8 -> IO ()
pingSubnet trace sock ipaddr@(IPv4 host) mask = do
	trace $ show (here, "Begin", mask, ipaddr)
	let pingList = ipv4SubnetPingList host mask --FIXME FIXME check if subnet is not already being scanned
	forM_ pingList $ \h -> do
		sendPing sock h
--		trace $ show (here, IPv4 h)
--		print (here, IPv4 h)
		--threadDelay 1000
	trace $ show (here, "Done", ipaddr, length pingList)
pingSubnet trace _ ip mask = trace $ show (here, ip, mask, "IGNORED!")

--pingNewSubnets :: NetInfoSocket -> IO ()
--pingNewSubnets = undefined


startPingThread :: NetInfoSocket -> IO PingSocket
startPingThread nis = do
	pingQ <- newTQueueIO

#if 0
	tr <- startTraceThread
	let qputstr = putTrace tr
#else
	let qputstr _ = return ()
#endif
	thread <- forkIO $ withPingSocket Nothing $ \sock -> do
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

	return $ PingSocket nis thread pingQ

--TODO TODO action to trigger ping all / ping specified subnet
--TODO TODO TODO periodic re-scan

data PingSocket = PingSocket NetInfoSocket ThreadId (TQueue (IP, Word8))

-- |
withPingNewSubnets ::
	NetInfoSocket -- ^
	-> (PingSocket -> IO a) -- ^
	-> IO a -- ^
withPingNewSubnets nis body = do
--	thread <- forkIO $ withPingSocket Nothing $ \sock -> do
--		return ()

	ps@(PingSocket _ thread _) <- startPingThread nis

--thread <- forkIO
	y <- withPingSocket Nothing $ \sock -> do
		body ps
	killThread thread -- FIXME use bracket
	return y

--------------------------------------------------------------------------------

