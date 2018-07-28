
module System.Linux.NetInfo
	(
-- * Types
	  Iface(..), IfNet(..), IP(..), Remote(..), IfMap
-- * Obtaining network informations
	, getNetInfo
-- * Watching network subsystem
	, NetInfoSocket
	, Event(..)
	, startNetInfo
	, stopNetInfo
	, withNetInfo
	, queryNetInfo
	, netInfoVarSTM
	, netInfoEventsSTM
	, newsLoop
-- * Utility functions
	, getSubnets
	, dumpIfMap
) where

import System.Linux.NetInfo.Internal

import System.Linux.Netlink
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception

--TODO if library switch to polling, it should generate events by itself

--------------------------------------------------------------------------------

-- | Handle to netlink socket
data NetInfoSocket = NIS
	{ nisNetInfo :: !(TVar IfMap)
	, nisEvents :: !(TChan Event)
	, nisThread :: !(ThreadId)
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

-- | 'startNetInfo' and 'stopNetInfo' conveniently combined with 'Control.Exception.bracket'
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

-- | Returns STM channel providing stream of 'Event'
netInfoEventsSTM :: NetInfoSocket -> IO (TChan Event)
netInfoEventsSTM = (atomically . dupTChan) . nisEvents

-- | Convenience function that returns snapshot of network informations
getNetInfo :: IO IfMap
getNetInfo = withNetInfo queryNetInfo  --FIXME won't work due to recvOne in receiveNews

--------------------------------------------------------------------------------

-- | event is Nothing on first call and when NetInfo has to resort to polling
newsLoop ::
	NetInfoSocket -- ^ socket to listen on
	-> ((Maybe Event, IfMap) -> IO (Maybe a)) -- ^ return Just to stop looping
	-> IO a -- ^ returns value that callback returned
newsLoop nis callback
	= atomically (dupTChan $ nisEvents nis)
	>>= \chan -> atomically readNM
	>>= \nm -> callback (Nothing, nm)
	>>= maybe (loop chan) return
	where
	readNM = readTVar (nisNetInfo nis)
	loop eventChan
		= atomically (
			(,) <$> Just <$> readTChan eventChan
			<*> readNM)
		>>= callback
		>>= maybe (loop eventChan) (return)

