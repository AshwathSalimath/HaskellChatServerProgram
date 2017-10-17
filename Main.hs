module Main where

import Network.Socket
import System.IO

main :: IO()

main = do
    sock <- socket AF_INET Stream 0             -- Creating a socket
    setSocketOption sock ReuseAddr 1            -- Make socket reusable Immediately -- easy debugging
    bind sock (SockAddrInet 4245 iNADDR_ANY)    -- Binding sock with TCP port number 4245
    listen sock 2                               -- Setting a max of 2 queued connections
    mainLoop sock                               -- mainLoop will be responsible for accepting a connection,
                                                -- running the Server logic, closing the connection,
                                                -- and passing the control back to the Original server


mainLoop :: Socket -> IO()
mainLoop sock = do
    conn <- accept sock                         -- Accepting a connection and handling it
    runConn conn                                -- Passing the conn (Socket object OR sock connection) to Server
                                                -- logic code
    mainLoop sock                               -- Repeat the process

-- Changing runnConn method as Network.Socket recommends to use ByteString Module but we will use System.IO for
-- simplicity; Turning our Socket into handle
runConn :: (Socket, SockAddr) -> IO()           -- Accepting a conn has a return type
runConn (sock, _) = do                          -- sock and SockAddr (port number) is not mentioned
    -- send sock "Hello\n"                         -- Sending a string "Hello" to the client
    -- close sock                                  -- Closing the socket connection
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello!"
    hClose hdl






