{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables, CPP, RecordWildCards, LambdaCase #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Control.Monad
import Control.Concurrent.STM
import System.Posix.Process
import Control.Concurrent

import System.Linux.Netlink
import System.Linux.Netlink.Constants
import qualified Data.Map as M
import Data.Word
--import qualified Data.Set as S

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import qualified Data.ByteString as B

import Numeric (showHex)
import Data.Bits
import Control.Exception

import Data.Serialize

import System.Linux.NetInfo
import System.Linux.Ping


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
			let x = [1..(2^(32 - mask))-2] --XXX obviously i don't fully understand the range of network address
			let y = fmap (htonl. (+host_)) x
	--		print (here, sock)
			forM_ y $ \h -> do
--				trace $ show (here, h)
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

{-

fping -ag 192.168.0.0/24

sudo -s -H
echo 8388608 > /proc/sys/net/core/rmem_max

-}

--------------------------------------------------------------------------------

dump :: (String -> IO ()) -> IfMap -> IO ()
dump put updated = forM_ (M.toList updated) $ \(ifIndex, Iface ifName mac nets remotes up) -> do
	put $ show ifIndex ++ " " ++ ifName ++ " " ++ show mac ++
		(if up then " UP" else " DOWN")
--		(show up)
	put $ "\tnets:"
	forM_ (M.toList nets) $ \(ip, IfNet mask) -> do
		put $ "\t" ++ show ip ++ "/" ++ show mask
	put $ "\tremotes:"
	forM_ (M.toList remotes) $ \(mac, ips) -> do
		put $ "\t" ++ show mac
		forM_ ips $ \(Remote ip) -> do
			put $ "\t\t" ++ show ip

--------------------------------------------------------------------------------

newSubnet'' :: TQueue (IP, Word8) -> IP -> Word8 -> IO () --ping all IPs in subnet
newSubnet'' q ip mask = atomically $ writeTQueue q (ip, mask)

qtrace_ :: Show a => TQueue String -> a -> IO ()
qtrace_ q x = atomically $ writeTQueue q $ show x

loop s f = f s >>= f

recvOne_ note trace sock
	= catch (recvOne sock) $ \e -> do
		trace $ show (here, "RECV FAILURE", note, (e :: IOException))
		return []

main :: IO ()
main = do
--http://elixir.free-electrons.com/linux/v4.15-rc5/source/include/uapi/linux/netlink.h
	sock <- makeSocketGeneric eNETLINK_ROUTE

	pid <- getProcessID

#if 1
	joinMulticastGroup sock eRTNLGRP_LINK
	joinMulticastGroup sock eRTNLGRP_IPV4_IFADDR
--	joinMulticastGroup sock eRTNLGRP_NEIGH
#endif
	traceQ <- newTQueueIO
	forkIO ( forever $ atomically (readTQueue traceQ) >>= putStrLn )

	let qputstr x = (atomically $ writeTQueue traceQ x) ::  (IO ())
	let qtrace = qtrace_ traceQ --(qputstr . show) ::  (Show a => a -> IO ())

	pingPending <- newTVarIO 0
	subnets <- newTVarIO []
	
	pingQ <- newTQueueIO
	let newSubnet e = case e of
		NewSubnet ip mask -> newSubnet'' pingQ ip mask
		DelSubnet ip mask -> error here --TODO remove from ping thread thread rotation


	let newSubnet2 e = case e of
		NewSubnet ip mask -> do
			atomically $ do
--				s <- readTVar subnets
				modifyTVar subnets $ let x = (ip, mask) in (x:) . filter (==x)
--				error here
		DelSubnet ip mask -> do
			atomically $ do
--				s <- readTVar subnets
--				writeTVar subnets $
				modifyTVar subnets $ filter (==(ip, mask))
--			error here --TODO remove from ping thread thread rotation

	forkIO $
		withPingSocket Nothing $ \sock -> do

			forkIO $ forever $ do
				(s, src) <- recvFrom sock 40
				atomically $ modifyTVar pingPending $ \case
					0 -> 0
					x -> x - 1
--					x | x > 0 -> x - 1
--					x -> x
				let msg = runGet (ipv4Packet getICMPHeader) s
--				print (here, src, B.length s, "received:", runGet (ipv4Packet getICMPHeader) s)
				case msg of
					Right IcmpHeader{icmp_type = 0, code = 0} -> do
						qputstr $ show (here, "ping response from", src)
					_ -> return ()

			forever $ do
				(ip, netmask) <- atomically $ readTQueue pingQ
				atomically $ modifyTVar pingPending (+1)
				qtrace (here, "!!!!!!!!!! ONE !!!!!!!!!!!", ip, netmask)
				atomically $ readTVar pingPending >>= (guard . ( <= 8))
				qtrace (here, "!!!!!!!!!!! TWO !!!!!!!!!!", ip, netmask)
				newSubnet''' qputstr sock ip netmask
				qtrace (here, "!!!!!!!!!!! THREE !!!!!!!!!!", ip, netmask)

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
--	query sock queryGetLink >>= mapM_ (handleNews''' trace noPing nm)
--	query sock queryGetAddr >>= mapM_ (handleNews''' trace noPing nm)
--	query sock queryGetNeigh >>= mapM_ (handleNews''' trace noPing nm)

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
--			>>= print

--		initial <- atomically $ readTVar nm

--		let subnets = concat $ fmap (fmap (fmap ifnLength) . M.toList . ifaceNets) $ M.elems initial
--		print (here, subnets)
--XXX XXX XXX
--		forM_ subnets $ uncurry newSubnet
--		print (here)

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
		qputstr ">>>"
		dump qputstr updated
		qputstr "<<<"
#endif
		return ()

	forever getLine

	closeSocket sock

	where
	noPing :: a -> b -> IO ()
	noPing = \_ _ -> pure ()

	noTrace :: String -> IO ()
	noTrace = const $ return ()


