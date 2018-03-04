{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables, CPP, RecordWildCards, ExistentialQuantification #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Control.Monad
import Control.Concurrent.STM
--import System.Posix.Process
import Control.Concurrent
--import qualified Data.Map as M
import Data.Word
import Network.Socket hiding (sendTo, recvFrom)
--import Network.Socket.ByteString ( recvFrom)
--import qualified Data.ByteString as B
--import Numeric (showHex)
--import Data.Bits
import Control.Exception
--import Data.Serialize

import System.Console.ANSI (clearScreen)
import System.Environment (getArgs)

--import System.Linux.Netlink
--import System.Linux.Netlink.Constants

import System.Linux.NetInfo
import System.Linux.Ping


--https://serverfault.com/questions/648140/how-to-scan-ipv6-enabled-hosts-on-my-lan
--https://superuser.com/questions/1135757/scanning-in-ipv6

--------------------------------------------------------------------------------


--FIXME FIXME check if subnet is not already being scanned
newSubnet''' :: (String -> IO ()) -> Socket -> IP -> Word8 -> IO () --ping all IPs in subnet
#if 0
newSubnet''' _ _ ip@[127,0,0,d] mask = return ()

newSubnet''' trace sock ip@[a,b,c,d] mask = do
	let host = tupleToHostAddress (a,b,c,d)
	let m = shiftL maxBound (32 - fromIntegral mask)
	let host_ = ntohl host .&. m

--	print (here, ip, mask, host, showHex host "", showHex m "")
#if 1
--	forkIO $ do
--		threadDelay 2000000
	do
--		bracket (pingSocket $ Just $ SockAddrInet 0 host) close $ \sock -> do

			trace $ show (here, "Begin", ip, mask, host, showHex host "", showHex m "")
			let x = [1..(2^(32 - mask))-2]
			let y = fmap (htonl. (+host_)) x
	--		print (here, sock)
			forM_ y $ \h -> do
				sendPing sock h
				trace $ show (here, h)
--				threadDelay 1000
			trace $ show (here, "Done", showHex host "", length x)
			return ()
#endif
	return ()
newSubnet''' trace _ ip mask = do
	trace $ show (here, ip, mask, "IGNORED!")
#else
newSubnet''' _ _ _ _ = return ()
#endif

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

	forkIO $ withPingSocket Nothing $ \sock -> do
#if 0
-- FIXME do not let sock leave the bracket
	forkIO $ forever $ do
		(s, src) <- recvFrom sock 40
		atomically $ modifyTVar pingPending $ \case
			0 -> 0
			x -> x - 1
		let msg = runGet (ipv4Packet getICMPHeader) s
--				print (here, src, B.length s, "received:", runGet (ipv4Packet getICMPHeader) s)
		case msg of
			Right IcmpHeader{icmp_type = 0, code = 0} -> do
				qputstr $ show (here, "ping response from", src)
			_ -> return ()
#endif

		forever $ do
			(ip, netmask) <- atomically $ readTQueue pingQ
--				atomically $ modifyTVar pingPending (+1)
--				atomically $ readTVar pingPending >>= (guard . ( <= 8))
			let qputstr _ = return ()
			newSubnet''' qputstr sock ip netmask


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

