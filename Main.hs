module Main where

import Network.Socket

main :: IO()

main = do
    sock <- socket AF_INET Stream 0             -- Creating a socket
    setSocketOption sock ReuseAddr 1            -- Make socket reusable Immediately -- easy debugging
    bind sock (SockAddrInet 4245 iNADDR_ANY)    -- Binding sock with TCP port number 4245
    listen sock 2                               -- Setting a max of 2 queued connections
    mainLoop sock                               -- mainLoop will be responsible for accepting a connection,
                                                -- running the Server logic, closing the connection,
                                                -- and recursing back to the Original server


mainLoop :: Socket -> IO()
mainLoop sock = do
    conn <- accept sock
    runConn conn


