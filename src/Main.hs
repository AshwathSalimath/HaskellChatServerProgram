module Main where

import Network.Socket
import System.IO -- Used for I/O
import Control.Concurrent -- Used for lightweight thread creation and context switching
import Control.Monad.Fix (fix)  -- Fix allows us to define Monadic fixpoint
                                -- Monad is a Design Pattern
import Control.Monad (when)

-- Monad Design Pattern

type Msg = (Int, String)
main :: IO()

main = do
    sock <- socket AF_INET Stream 0             -- Creating a socket
    setSocketOption sock ReuseAddr 1            -- Make socket reusable Immediately -- easy debugging
    bind sock (SockAddrInet 4245 iNADDR_ANY)    -- Binding sock with TCP port number 4245
    listen sock 2                               -- Setting a max of 2 queued connections
    chan <- newChan                             -- Creating a FIFO Channel which will be passed to mainLoop and runConn
    _ <- forkIO $ fix $ \loop -> do
        (_, _) <- readChan chan
        loop
    mainLoop sock chan 0                        -- mainLoop will be responsible for accepting a connection,
                                                -- running the Server logic, closing the connection,
                                                -- and passing the control back to the Original server


mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock                         -- Accepting a connection and handling it
    -- runConn conn                             -- Passing the conn (Socket object OR sock connection) to Server
                                                -- logic code
    forkIO (runConn conn msgNum)                -- splitting off each connection into its own thread
    mainLoop sock chan $! msgNum + 1            -- Repeat the process

-- Changing runConn method as Network.Socket recommends to use ByteString Module but we will use System.IO for
-- simplicity; Turning our Socket into handle
runConn :: (Socket, SockAddr) -> Chan Msg -> IO()           -- Accepting a conn has a return type
runConn (sock, _) chan = do                          -- sock and SockAddr (port number) is not mentioned
    -- send sock "Hello\n"                         -- Sending a string "Hello" to the client
    -- close sock                                  -- Closing the socket connection
    let broadcast msg = writeChan chan msg         -- Writing msg to chan FIFO channel
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan                       -- creating commLine which is a duplicate chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
            -- If an exception is caught, send a message and break the loop
            "quit" -> hPutStrLn hdl "Bye!"
            -- else, continue looping.
            _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl

-- Now Adding Concurrency so that the server can handle more than one client ;
-- fork new thread for each Client connection
-- We will make use of Control.Concurrent.Chan module add communication between threads
-- It provides Unbounded FIFO Channel with Single Write and Multiple Read













