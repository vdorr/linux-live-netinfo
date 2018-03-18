{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports  -fwarn-incomplete-patterns -fno-warn-tabs #-}
{-# LANGUAGE CPP, RecordWildCards  #-} 
#define here (__FILE__ ++ ":" ++ show __LINE__ ++ " ")

module System.Linux.Ping where

import Data.Word
import qualified Data.ByteString as B
import Data.Serialize
import Data.Bits

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Control.Exception
import System.Posix.Process
import Data.Maybe (fromMaybe)
--import Control.Concurrent

--------------------------------------------------------------------------------

data IcmpHeader = IcmpHeader
	{ icmp_type
	, code :: Word8
	, header_checksum
	, ident --this has to be current process id (System.Posix.getProcessID)
	, seq :: Word16
	} deriving (Show)

putHeader :: IcmpHeader -> B.ByteString -> Put
putHeader IcmpHeader{..} icmp_data = do
	putWord8 icmp_type
	putWord8 code
	putWord16be header_checksum
	putWord16be ident
	putWord16be seq
	putByteString icmp_data

getICMPHeader :: Get IcmpHeader
getICMPHeader = IcmpHeader
	<$> getWord8
	<*> getWord8
	<*> getWord16be
	<*> getWord16be
	<*> getWord16be

listOfWord16 :: Get [Word16]
listOfWord16 = do
	empty <- isEmpty
	if empty
	then return []
	else do v <- getWord16be
		rest <- listOfWord16
		return (v : rest)

checksum :: B.ByteString -> Word16
-- Calculate checksum of an ICMP packet.
-- Implementation stolen from http://programatica.cs.pdx.edu/House/
checksum bs = let
	bs' = if B.length bs `mod` 2 == 0
		then bs
		else B.snoc bs 0
	Right ws = runGet listOfWord16 bs' --FIXME pattern
	total = sum (map fromIntegral ws) :: Word32
	in complement (fromIntegral total + fromIntegral (total `shiftR` 16))

echo_request :: Word8
echo_request = 8

buildPacket :: Word16 -> Word16 -> B.ByteString -> B.ByteString
-- First build a header with checksum=0 then calculate the checksum and
-- put it into the header.
buildPacket ident seq icmp_data = buildPacket' $ checksum $ buildPacket' 0
	where
	buildPacket' cs = runPut $ putHeader (IcmpHeader echo_request 0 cs ident seq) icmp_data

ipV4header :: Get (Word8, Word8, Word16)
ipV4header = (,,) <$> getWord8 <*> getWord8 <*> getWord16be

ipv4Packet payload = do
	ver_ihl <- getWord8
	let ihl = ver_ihl .&. 0xf --header length
	getWord8
	len <- getWord16be
	skip (fromIntegral ((ihl - 1) * 4))
	-- ...
	payload

--------------------------------------------------------------------------------

-- | 127.0.0.1 in host format
localIPv4 :: Word32
localIPv4 = tupleToHostAddress (127,0,0,1)

-- | List of all IPs in given subnet worth pinging, localhost is ignored
ipv4SubnetPingList :: Word32 -> Word8 -> [Word32]
ipv4SubnetPingList host mask
	| host == localIPv4 = []
	| otherwise = let--ipv4Range host mask 
		m = shiftL maxBound (32 - fromIntegral mask)
		host_ = ntohl host .&. m
		x = [1..(2^(32 - mask))-2]
		in fmap (htonl . (+host_)) x

--------------------------------------------------------------------------------

--ipv4Range' :: (Word8, Word8, Word8, Word8) -> Word8 -> [Word32]
--ipv4Range' ip = ipv4Range (ntohl $ tupleToHostAddress ip)

ipv4Range :: Word32 -> Word8 -> [Word32]
ipv4Range host mask = fmap (htonl . (network+)) range
	where
	bitMask = shiftL maxBound (32 - fromIntegral mask)
	network = host .&. bitMask
	range = [1..(2^(32 - mask))-2]

pingSocket :: Maybe SockAddr -> IO Socket
pingSocket localAddress = do
	sock <- socket AF_INET Raw 1
--	sock <- socket AF_INET Datagram 1

	fromMaybe (return ()) (fmap (bind sock) localAddress) --XXX probably should be bound, but i should check on that
	--XXX so_reuse?
	return sock

withPingSocket :: Maybe SockAddr -> (Socket -> IO a) -> IO a
withPingSocket local f = bracket (pingSocket local) close f

sendPing :: Socket -> HostAddress -> IO Int
sendPing sock dst = do
	pid <- getProcessID
	let packet = buildPacket (fromIntegral pid) 0 B.empty
	let addr = SockAddrInet 0 dst
	catch (sendTo sock packet addr) $ \e -> do
		print (here, addr, e :: IOException)
--		threadDelay 1000
		return (-1)

receivePingFrom :: Socket -> IO (Either String IcmpHeader, SockAddr)
receivePingFrom sock = do
	(s, src) <- recvFrom sock 40
	return $ (runGet (ipv4Packet getICMPHeader) s, src)


