{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module System.Linux.NetInfo.Probe (
	  PingSocket
	, withPingNewSubnets
	, pingAll
	, pingSubnet
) where

import System.Linux.NetInfo
import System.Linux.Ping

import Network.Socket
import Data.Word
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import Data.List
import Control.Applicative

--------------------------------------------------------------------------------

stmTimeout' :: Int -> a -> STM a -> IO a
stmTimeout' microseconds defVal f
	= registerDelay microseconds
	>>= \timeouted -> atomically $
		f
		<|> defVal <$ (readTVar timeouted >>= check)

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

			(msg, src) <- receivePingFrom sock
			case msg of
				Right IcmpHeader{icmp_type = 0, code = 0} -> do
					qputstr $ show (here, "ping response from", src)
				_ -> return ()
#endif

		events <- netInfoEventsSTM nis
		subnets <- getSubnets <$> queryNetInfo nis
--		print (here, subnets)

		loop subnets $ \pingList -> do
			print (here, pingList)

			event <- stmTimeout' 200000 Nothing $ Just <$> readTChan events
--			print (here, event)
			case event of
				Just (AddSubnet _ ip (IfNet mask))
					-> pingSubnet qputstr sock ip mask
					>> let s = (ip, mask)
						in return (delete s pingList ++ [s])
				Just (DelSubnet _ ip (IfNet mask))
					-> return (delete (ip, mask) pingList)
				_ -> case pingList of
					(ip, mask):xs
						-> pingSubnet qputstr sock ip mask
						>> return (xs ++ [(ip, mask)])
					[] -> return []

	return $ PingSocket nis thread pingQ
	where
	loop l f = () <$ (f l >>= flip loop f)


data PingSocket = PingSocket NetInfoSocket ThreadId (TQueue (IP, Word8))

pingAll :: PingSocket -> IO ()
pingAll (PingSocket nis _ pingQ) = do
	subnets <- getSubnets <$> queryNetInfo nis
	forM_ subnets $ \(ip, mask) ->
		atomically $ writeTQueue pingQ (ip, mask)

killPingThread :: PingSocket -> IO ()
killPingThread (PingSocket _ thread _) = killThread thread

-- | Spawns a ping thread and pass its handle to inner action, thread gets killed when action ends or throws an exception.
withPingNewSubnets ::
	NetInfoSocket -- ^
	-> (PingSocket -> IO a) -- ^
	-> IO a -- ^
withPingNewSubnets nis body
	= bracket (startPingThread nis) killPingThread body

--------------------------------------------------------------------------------

