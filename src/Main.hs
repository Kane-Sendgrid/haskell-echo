import Network.Socket
import System.IO
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Concurrent
import Debug.Trace
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple

comp = "LoggingExample.Main"

main :: IO ()
main = do
    sh <- openlog "LoggingExample" [PID] LOCAL0 NOTICE
    updateGlobalLogger comp $ addHandler sh

    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    forkIO(runConn conn)
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) =
    let loop hdl = do
        ineof <- hIsEOF hdl
        case ineof of
            True -> return ()
            _    -> do
                traceIO "test"
                warningM comp "This buggy component is buggy"
                line <- hGetLine hdl
                send sock (line ++ "\n")
                loop hdl
    in do
        hdl <- socketToHandle sock ReadWriteMode
        loop hdl
        hClose hdl