{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables, CPP, RecordWildCards #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module System.Linux.NetInfo
	( IfMap, Iface(..), IfNet(..), IP, Remote(..)
	, Event(..)
#if 1
	, NetInfoSocket
	, startNetInfo
	, stopNetInfo
	, withNetInfo
	, queryNetInfo
	, netInfoVarSTM
	, netInfoEventsSTM
	, newsLoop
	, getNetInfo
#endif
	, translateNews, translateNewsA
	, mergeNews
	, emptyNMap
	, queryGetLink, queryGetAddr, queryGetNeigh
	, collectNews
	, tapNetlink
	, dumpIfMap
) where

import System.Linux.Netlink
import System.Linux.Netlink.Route
import System.Linux.Netlink.Constants
import Data.Word
import qualified Data.Map as M
--import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Set as S
import Data.ByteString.Char8 (ByteString, unpack) --append, init, pack, 
import qualified Data.ByteString as B
import Data.Bits
import Data.Maybe
import Data.Char (ord)
import Control.Monad

import Control.Exception (bracket, catch, IOException)
import Data.Functor.Identity

--import Network.Socket
import Numeric (showHex)
import Data.List
import Data.Serialize

--------------------------------------------------------------------------------

-- | Handle to netlink socket
data NetInfoSocket = NIS
	{
-- nisSock :: NetlinkSocket ,
--	  nisSwitch :: TVar Bool	, 
	  nisNetInfo :: TVar IfMap
	, nisEvents :: TChan Event
	, nisThread :: ThreadId
--	, nisDone :: TMVar ()
	}

-- | Start watching network subsystem
startNetInfo :: IO NetInfoSocket
startNetInfo = do
	nm <- emptyNMap
	events <- newBroadcastTChanIO

	let updateNM = modifyTVar' nm . flip mergeNews
	let bulkUpdateNM = atomically . mapM_ updateNM

	initialized <- newEmptyTMVarIO

	thread <- forkIO $
--FIXME do initial query, structured so that no information is lost
--join link mcast, query link, join addr mcast, query ...
--leaveMulticast heuristic
		bracket tapNetlink closeSocket $ \sock ->
			askForNews queryGetLink sock >>= bulkUpdateNM
			>> askForNews queryGetAddr sock >>= bulkUpdateNM
			>> askForNews queryGetNeigh sock >>= bulkUpdateNM
			>> atomically (putTMVar initialized ())
			>> forever (receiveNews sock >>= \es ->
				forM_ es $ \e -> atomically $
					updateNM e
					>> writeTChan events e)
	atomically $ takeTMVar initialized
	return $ NIS nm events thread

withNetInfo :: (NetInfoSocket -> IO a) -> IO a
withNetInfo = bracket startNetInfo stopNetInfo

-- | Stop watching
stopNetInfo :: NetInfoSocket -> IO ()
stopNetInfo = killThread . nisThread

-- | Returns latest snapshot
queryNetInfo :: NetInfoSocket -> IO IfMap
queryNetInfo = readTVarIO . nisNetInfo

-- | Returns STM variable with state of network neighborhood
netInfoVarSTM :: NetInfoSocket -> TVar IfMap
netInfoVarSTM = nisNetInfo

-- | Returns STM channels providing stream of 'Event'
netInfoEventsSTM :: NetInfoSocket -> IO (TChan Event)
netInfoEventsSTM = (atomically . cloneTChan) . nisEvents

-- | Convenience function that returns snapshot of network informations
getNetInfo :: IO IfMap
getNetInfo = withNetInfo queryNetInfo

--------------------------------------------------------------------------------

-- | event is Nothing on first call and when NetInfo has to resort to polling
newsLoop ::
	NetInfoSocket -- ^ socket to listen on
	-> ((Maybe Event, IfMap) -> IO (Maybe a)) -- ^ return Just to stop looping
	-> IO a -- ^ returns value that vallback returned
newsLoop nis callback
	= atomically (dupTChan $ nisEvents nis)
	>>= \chan -> atomically readNM
	>>= \nm -> print here >> callback (Nothing, nm)
	>>= maybe (loop chan) return
	where
	readNM = readTVar (nisNetInfo nis)
	loop eventChan
		= atomically (
			(,) <$> Just <$> readTChan eventChan
			<*> readNM)
		>>= callback
		>>= maybe (loop eventChan) (return)

askForNews :: Packet Message -> NetlinkSocket -> IO [Event]
askForNews q sock = catMaybes <$> (translateNews <$>) <$> query sock q

receiveNews :: NetlinkSocket -> IO [Event]
receiveNews sock = do
	news <- (translateNews <$>) <$> recvOne sock
	case catMaybes news of
		[] -> receiveNews sock
		news' -> return news'

#if 0
recvOne_ note trace sock
	= catch (recvOne sock) $ \e -> do
		trace $ show (here, "RECV FAILURE", note, (e :: IOException))
		return []
#endif

--------------------------------------------------------------------------------

getFlags :: FiniteBits b => b -> [Int]
getFlags x = catMaybes $ fmap (\bit -> if testBit x bit then Just bit else Nothing )
	[ 0 .. finiteBitSize x - 1 ]

#if 0
getAttr :: Enum a => a -> Attributes -> Maybe ByteString
getAttr a = M.lookup (fromEnum a)
#endif

--------------------------------------------------------------------------------

queryGetLink :: Packet Message
queryGetLink
	= let h = Header
		{ messageType = eRTM_GETLINK
		, messageFlags = fNLM_F_REQUEST + (fNLM_F_ROOT + fNLM_F_MATCH)
		, messageSeqNum = 1
		, messagePID = 0
		}
	in Packet h (NLinkMsg 0 0 0) M.empty


queryGetAddr :: Packet Message
queryGetAddr
	= let h = Header
		{ messageType = eRTM_GETADDR
		, messageFlags = fNLM_F_REQUEST + (fNLM_F_ROOT + fNLM_F_MATCH)
		, messageSeqNum = 1
		, messagePID = 0
		}
	in Packet h
--		(NLinkMsg 0 0 0)
		(NAddrMsg 0 0 0 0 0)
		M.empty

queryGetNeigh :: Packet Message
queryGetNeigh = let
	h = Header
		{ messageType = eRTM_GETNEIGH
		, messageFlags = fNLM_F_REQUEST + (fNLM_F_ROOT + fNLM_F_MATCH)
		, messageSeqNum = 1
		, messagePID = 0
		}
--define NUD_REACHABLE	0x02
	ndm = NNeighMsg
		{ neighFamily = eAF_INET
		, neighIfindex = 0
		, neighState = fNUD_REACHABLE --FIXME FIXME FIXME this setting would suppress disappearence of neighbor!
		, neighFlags = 0
		, neighType = 0
		}
	in Packet h ndm M.empty

--------------------------------------------------------------------------------

-- | MAC address
newtype LinkAddress = LinkAddress (Word8, Word8, Word8, Word8, Word8, Word8)
	deriving (Ord, Eq)

instance Show LinkAddress where
	show (LinkAddress (a, b, c, d, e, f))
		= intercalate ":" $ fmap s [a, b, c, d, e, f]
		where
			s x = pad '0' 2 (showHex x "")
			pad c l = reverse . take l . (flip (++) (replicate l c)) . reverse

-- | Map from interface index to interface state
type IfMap = M.Map Word32 Iface -- iface index

-- | State of interface
data Iface = Iface
	{ ifaceName :: String -- ^ name
	, ifaceAddr :: LinkAddress -- ^ MAC address
	, ifaceNets :: M.Map IP IfNet -- ^ map from IP addresses to subnet size
	, ifaceRemotes :: M.Map LinkAddress (S.Set Remote) -- ^ map from MAC of remote device to set of remote IPs
	, ifaceUp :: Bool -- ^ interface is Up
--	, ifFlags :: [Int]
	} deriving (Show, Eq, Ord)

-- | Netmask
data IfNet = IfNet { ifnLength :: Word8 } --netmask
	deriving (Show, Eq, Ord)

data Remote = Remote IP
	deriving (Show, Eq, Ord)

-- | IP, 4 or 6
data IP
	= IPv4 (Word8, Word8, Word8, Word8) -- ^ goes well with 'Network.Socket.tupleToHostAddress'
	| IPv6 (Word32, Word32, Word32, Word32) -- ^ possibly (?) goes well with 'Network.Socket.tupleToHostAddress6'
	deriving (Eq, Ord)

instance Show IP where
	show (IPv4 (a, b, c, d)) = intercalate "." $ fmap show [a, b, c, d]
	show (IPv6 (a, b, c, d)) = "this is ipv6:" ++ show (a, b, c, d) --FIXME FIXME

--------------------------------------------------------------------------------

decodeIP :: ByteString -> Either String IP
decodeIP s
	| len == 4 = IPv4 <$> runGet
		((,,,) <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8) s
	| len == 16 = IPv6 <$> runGet
		((,,,) <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host) s
	| otherwise = Left $ "unusual address length: " ++ show len
	where
	len = B.length s

--decodeIP :: ByteString -> [Word8]
--decodeIP s = map (fromIntegral . ord) $ unpack s

getIfIPAttr :: Attributes -> Either String IP
getIfIPAttr attr = maybe (Left "no IF addr attribute") decodeIP $ getIFAddr attr

getNeighDstAttr :: Attributes -> Either String IP
getNeighDstAttr attr = maybe (Left "no Dst attribute") decodeIP $ getDstAddr attr

--attr = decodeIP <$> M.lookup eIFA_ADDRESS attr
--	getLLAddr attr = decodeMAC <$> getAttr eNDA_LLADDR attr
--	getDstAddr attr = decodeIP <$> getAttr eNDA_DST attr

#if 0
	addr addrFamily
		| addrFamily == eAF_INET = show . decodeIPv4
		| addrFamily == eAF_INET6 = show . decodeIPv6
		| otherwise = error here
#endif

--------------------------------------------------------------------------------

data Event
	= AddSubnet Word32 IP IfNet -- ^ New IP address added on interface identified by index
	| DelSubnet Word32 IP IfNet

	| AddIface Word32 String LinkAddress Bool -- ^ index, name, mac, is up
	| DelIface Word32
--TODO	| UpdateIface Word32 String Bool -- ^ Mainly up/down

	| AddNeigbour Word32 LinkAddress Remote
	| DelNeighbour Word32 LinkAddress Remote

	| ErrorEvent String
	deriving (Show, Eq, Ord)


translateNews :: Packet Message -> Maybe Event
translateNews = runIdentity . translateNewsA (const (pure ()))

translateNewsA :: Applicative a =>
		(String -> a ())
                      -> Packet Message
                      -> a (Maybe Event)
translateNewsA trace (err@ErrorMsg {})
	= trace (show (here, err))
	*> pure (Just $ ErrorEvent $ show err)
translateNewsA _ (DoneMsg {})
	= pure Nothing
translateNewsA trace (Packet Header{..} NLinkMsg{..} attr) --for flags see man netdevice
	| messageType == eRTM_NEWLINK, Just name <- getLinkName attr, Just mac <- getLinkAddress attr
		= trace (show (here, "add iface", interfaceIndex, getLinkName attr, "flags:", getFlags interfaceFlags, mac))
		*> pure (Just $ AddIface interfaceIndex name (LinkAddress mac) (interfaceFlags .&. fIFF_UP /= 0))
	| messageType == eRTM_DELLINK
		= trace (show (here, "remove iface", interfaceIndex, getLinkName attr))
		*> pure (Just $ DelIface interfaceIndex)
	| otherwise
		= trace (show (here, showMessageType messageType, getLinkName attr, getLinkAddress attr, testBit interfaceFlags fIFF_UP))
		*> pure Nothing
translateNewsA trace (Packet Header{..} NAddrMsg {..} attr)
	| messageType == eRTM_NEWADDR, Right ip <- getIfIPAttr attr
		= trace (show (here, "add address", "ifi:", addrInterfaceIndex, "mask:", addrMaskLength, ip))
		*> pure (Just $ AddSubnet addrInterfaceIndex ip (IfNet addrMaskLength))
	| messageType == eRTM_DELADDR, Right ip <- getIfIPAttr attr
		= trace (show (here, "remove address", "ifi:", addrInterfaceIndex, "mask:", addrMaskLength, ip))
		*> pure (Just $ DelSubnet addrInterfaceIndex ip (IfNet addrMaskLength))
	| otherwise
		= trace (show (here, showMessageType messageType, getIfIPAttr attr)) --should not happen
		*> pure Nothing
translateNewsA trace (Packet Header{..} NNeighMsg{..} attr)
	| messageType == eRTM_NEWNEIGH, Just mac <- getLLAddr attr, Right ip <- getNeighDstAttr attr
--		, testBit neighState fNUD_REACHABLE
		= trace (show (here, "add neighbor", "ifi:", neighIfindex, mac, ip, getFlags neighState))
		*> pure (Just $ AddNeigbour (fromIntegral neighIfindex) (LinkAddress mac) $ Remote ip) --FIXME uneasy feeling about fromIntegral
	| messageType == eRTM_DELNEIGH, Just mac <- getLLAddr attr, Right ip <- getNeighDstAttr attr
		= trace (show (here, "remove neighbor", "ifi:", neighIfindex, mac, ip))
		*> pure (Just $ DelNeighbour (fromIntegral neighIfindex) (LinkAddress mac) (Remote ip))
	| otherwise
		= trace (show (here, showMessageType messageType,
			getFlags neighState,
			getLLAddr attr,
			fmap (fmap ord.unpack) (getDstAddr attr) )) --should not happen
		*> pure Nothing

--------------------------------------------------------------------------------

mergeNews :: IfMap -> Event -> IfMap
mergeNews nm (ErrorEvent _) = nm
mergeNews nm (AddIface interfaceIndex name mac isUp)
	= M.insert interfaceIndex
		(Iface name mac M.empty M.empty isUp)
		nm
mergeNews nm (DelIface interfaceIndex)
	= M.delete interfaceIndex nm
mergeNews nm (AddSubnet addrInterfaceIndex ip ifNet)
	= M.adjust
		(\i -> i { ifaceNets =
			M.insert ip ifNet $ ifaceNets i })
		addrInterfaceIndex
		nm
mergeNews nm (DelSubnet addrInterfaceIndex ip _)
	= M.adjust
		(\i -> i { ifaceNets =
			M.delete ip $ ifaceNets i })
		addrInterfaceIndex
		nm
mergeNews nm (AddNeigbour neighIfindex mac remote)
	= M.adjust
		(\i -> i { ifaceRemotes =
			M.insertWith S.union mac (S.singleton remote) $ ifaceRemotes i })
		neighIfindex
		nm
mergeNews nm (DelNeighbour neighIfindex mac remote)
	= M.adjust
		(\i -> i { ifaceRemotes =
			M.alter
				(\remotes -> case fmap (S.delete remote) remotes of
					Just s' | not $ S.null s' -> Just s'
					_ -> Nothing )
				mac $ ifaceRemotes i })
		(fromIntegral neighIfindex)
		nm

--------------------------------------------------------------------------------

emptyNMap :: IO (TVar IfMap)
emptyNMap = newTVarIO M.empty

handleNews'' :: Monad m => (String -> m ()) -> (Event -> m ()) -> IfMap -> Packet Message -> m (Maybe IfMap)
handleNews'' trace newSubnet nm msg
	= translateNewsA trace msg
	>>= maybe (return Nothing) handleEvent
	where
	handleEvent event@AddSubnet {}
		= newSubnet event
		*> pure (Just $ mergeNews nm event)
	handleEvent event = pure $ Just $ mergeNews nm event

collectNews
  :: (String -> IO ())
     -> (Event -> IO ())
     -> TVar IfMap
     -> Packet Message
     -> IO Bool
collectNews trace newSubnet nmVar msg = do
	nm <- atomically $ readTVar nmVar
	nm' <- handleNews'' trace newSubnet nm msg
	case nm' of
		Just nm'' ->
			atomically $ writeTVar nmVar nm''
			*> pure True
		_ -> pure False

--------------------------------------------------------------------------------

tapNetlink :: IO NetlinkSocket
tapNetlink
	= makeSocketGeneric eNETLINK_ROUTE
	>>= \sock ->
#if 1
		joinMulticastGroup sock eRTNLGRP_LINK
		*> joinMulticastGroup sock eRTNLGRP_IPV4_IFADDR
		*> joinMulticastGroup sock eRTNLGRP_NEIGH
		*>
#endif
		pure sock

--------------------------------------------------------------------------------

--FIXME decouple from IO
dumpIfMap :: (String -> IO ()) -> IfMap -> IO ()
dumpIfMap put updated = forM_ (M.toList updated) $ \(ifIndex, Iface ifName mac nets remotes up) -> do
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

