{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables, CPP, RecordWildCards, LambdaCase #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Control.Monad
import Control.Concurrent.STM
--import System.Posix.Process
import Control.Concurrent

import System.Linux.Netlink
import System.Linux.Netlink.Constants
import qualified Data.Map as M
import Data.Word

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString ( recvFrom)
--import qualified Data.ByteString as B

import Numeric (showHex)
import Data.Bits
import Control.Exception

import Data.Serialize

import System.Linux.NetInfo
import System.Linux.Ping

import System.Console.ANSI (clearScreen)

import System.Environment (getArgs)

--https://serverfault.com/questions/648140/how-to-scan-ipv6-enabled-hosts-on-my-lan
--https://superuser.com/questions/1135757/scanning-in-ipv6

--------------------------------------------------------------------------------

#if 1
--FIXME FIXME check if subnet is not already being scanned
newSubnet''' :: (String -> IO ()) -> Socket -> IP -> Word8 -> IO () --ping all IPs in subnet
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
--				threadDelay 1000
			trace $ show (here, "Done", showHex host "", length x)
			return ()
#endif
	return ()
newSubnet''' trace _ ip mask = do
	trace $ show (here, ip, mask, "IGNORED!")
#endif

--------------------------------------------------------------------------------

dump :: (String -> IO ()) -> IfMap -> IO ()
dump put updated = forM_ (M.toList updated) $ \(ifIndex, Iface ifName mac nets remotes up) -> do
	put $ show ifIndex ++ " " ++ ifName ++ " " ++ show mac ++
		(if up then " UP" else " DOWN")
--		(show up)
	put $ "\tnets:"
	forM_ (M.toList nets) $ \(ip, IfNet mask) -> do
		put $ "\t\t" ++ show ip ++ "/" ++ show mask
	put $ "\tremotes:"
	forM_ (M.toList remotes) $ \(mac, ips) -> do
		put $ "\t\t" ++ show mac
		forM_ ips $ \(Remote ip) -> do
			put $ "\t\t\t" ++ show ip

--------------------------------------------------------------------------------

#if 0
noPing :: a -> b -> IO ()
noPing = \_ _ -> pure ()
#endif

noTrace :: String -> IO ()
noTrace = const $ return ()

newSubnet'' :: TQueue (IP, Word8) -> IP -> Word8 -> IO () --ping all IPs in subnet
newSubnet'' q ip mask = atomically $ writeTQueue q (ip, mask)

qtrace_ :: Show a => TQueue String -> a -> IO ()
qtrace_ q x = atomically $ writeTQueue q $ show x

recvOne_ note trace sock
	= catch (recvOne sock) $ \e -> do
		trace $ show (here, "RECV FAILURE", note, (e :: IOException))
		return []

--------------------------------------------------------------------------------

{-
fping -ag 192.168.0.0/24
-}

main :: IO ()
main = do

	args <- getArgs
	let active = take 1 args == ["--active"]

--http://elixir.free-electrons.com/linux/v4.15-rc5/source/include/uapi/linux/netlink.h

	sock <- tapNetlink

	traceQ <- newTQueueIO
	forkIO ( forever $ atomically (readTQueue traceQ) >>= putStrLn )

	let qputstr x = (atomically $ writeTQueue traceQ x) ::  (IO ())
--	let qtrace = qtrace_ traceQ --(qputstr . show) ::  (Show a => a -> IO ())

	pingPending <- newTVarIO 0
	subnets <- newTVarIO []
	
	pingQ <- newTQueueIO
	let newSubnet e = case e of
		AddSubnet _ ip (IfNet mask) -> newSubnet'' pingQ ip mask
		DelSubnet _ ip (IfNet mask) -> error here --TODO remove from ping thread thread rotation
		_ -> return ()

#if 0
	let newSubnet2 e = case e of
		AddSubnet _ ip (IfNet mask) -> do
			atomically $ do
--				s <- readTVar subnets
				modifyTVar subnets $ let x = (ip, mask) in (x:) . filter (==x)
--				error here
		DelSubnet _ ip (IfNet mask) -> do
			atomically $ do
--				s <- readTVar subnets
--				writeTVar subnets $
				modifyTVar subnets $ filter (==(ip, mask))
--			error here --TODO remove from ping thread thread rotation
		_ -> return ()
#endif

--XXX "ping mode" is enable by "--active"
	when active $ void $ forkIO $
		withPingSocket Nothing $ \sock -> do

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

			forever $ do
				(ip, netmask) <- atomically $ readTQueue pingQ
				atomically $ modifyTVar pingPending (+1)
				atomically $ readTVar pingPending >>= (guard . ( <= 8))
				newSubnet''' qputstr sock ip netmask

	let traceX = qputstr --putStrLn
#if 0
	let trace = qputstr --putStrLn
#else
	let trace = noTrace
#endif
	nm <- emptyNMap
	nmChanged <- newEmptyTMVarIO

#if 1
	let ping = newSubnet
#else
	let ping = noPing
#endif

	qtrace_ traceQ (here, "--------------------------------------------------------------------------------")

	forkIO $ do

		query sock queryGetLink >>= mapM_ (handleNews''' trace ping nm)
		query sock queryGetNeigh >>= mapM_ (handleNews''' trace ping nm)

--XXX only now join neigborhood group
		forkOS $ do
			sock2 <- makeSocketGeneric eNETLINK_ROUTE
			joinMulticastGroup sock2 eRTNLGRP_NEIGH
			forever $ recvOne_ here traceX sock2 >>= (mapM_ (handleNews'''
				trace (error here) nm))

		query sock queryGetAddr >>= mapM_ (handleNews''' trace ping nm)


		atomically (tryPutTMVar nmChanged ()) --trigger first dump

		forever $ do
			msg <- recvOne_ here traceX sock
--			print (here, msg)
			x <- foldM (\b a -> (b ||) <$> handleNews''' trace ping nm a) False msg
			when x $
				atomically $ void $ tryPutTMVar nmChanged ()
--			return ()

--	print (here, "--------------------------------------------------------------------------------")

--	atomically (tryPutTMVar nmChanged ()) --trigger first dump
--		>>= print
--	qtrace_ traceQ (here, "--------------------------------------------------------------------------------")

	forever $ do
--		query sock queryGetLink >>= mapM_ (handleNews''' trace ping nm)
--		query sock queryGetAddr >>= mapM_ (handleNews''' trace ping nm)
--		query sock queryGetNeigh >>= mapM_ (handleNews''' trace ping nm)
--		recvOne sock >>= mapM_ (handleNews''' trace ping nm)
--		updated <- atomically $ readTVar nm
--		dump updated
--		threadDelay 2000000

		updated <- atomically $ do
			takeTMVar nmChanged --wait for change in map
			readTVar nm
--			print (here, updated)
#if 1
		clearScreen
		qputstr "--------------------------------------------------------------------------------"
		dump qputstr updated
#endif
		return ()

	forever getLine

	closeSocket sock


