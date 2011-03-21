import Network
import Network.Socket 
import Control.Concurrent

type C_Socket = PrimData Socket

instance ConvertCurryHaskell C_Int PortID where
  toCurry (PortNumber i) = toCurry (toInteger i)
  fromCurry i = PortNumber (fromInteger (fromCurry i))

external_d_C_prim_listenOn :: C_Int -> C_IO C_Socket
external_d_C_prim_listenOn = fromHaskellIO1 listenOn

external_d_C_listenOnFresh :: C_IO (OP_Tuple2 C_Int C_Socket)
external_d_C_listenOnFresh = fromHaskellIO0 listenOnFreshPort
 where
   listenOnFreshPort :: IO (PortID,Socket)
   listenOnFreshPort = do
     s <- listenOn (PortNumber aNY_PORT)
     p <- Network.socketPort s
     return (p,s)

external_d_C_prim_socketAccept :: C_Socket -> C_IO (OP_Tuple2 C_String C_Handle)
external_d_C_prim_socketAccept =
 fromHaskellIO1 (\s -> Network.accept s >>= \ (h,s,_) -> return (s,OneHandle h))


external_d_C_prim_waitForSocketAccept :: C_Socket -> C_Int -> C_IO (C_Maybe (OP_Tuple2 (OP_List C_Char) C_Handle))
external_d_C_prim_waitForSocketAccept = fromHaskellIO2 wait

wait :: Socket -> Int -> IO (Maybe (String,CurryHandle))
wait s t = do
  mv <- newEmptyMVar
  tacc <- forkIO (Network.accept s >>= \ (h,s,_) ->
                  putMVar mv (Just (s,OneHandle h)))
  ttim <- forkIO (threadDelay (t*1000) >> putMVar mv Nothing)
  res <- takeMVar mv
  maybe (killThread tacc) (\_ -> killThread ttim) res
  return res

external_d_C_prim_sClose :: C_Socket -> C_IO OP_Unit
external_d_C_prim_sClose = fromHaskellIO1 sClose

external_d_C_prim_connectToSocket :: C_String -> C_Int -> C_IO C_Handle
external_d_C_prim_connectToSocket =
  fromHaskellIO2 (\ s i -> connectTo s i >>= return . OneHandle)
