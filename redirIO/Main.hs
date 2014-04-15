module Main where

import ParseArgs

import System.Environment
import Network
import System.IO
import System.Process


main = do
	args <- getArgs
	let options = parseArgs args
	case options of
		Left error -> putStr error
		Right options -> do
			streams <- streamsFromOptions (tcpPort options)
			startApp streams (applicationAndParams options)
			return ()

streamsFromOptions maybePort = case maybePort of
	Nothing -> return (stdin, stdout, stderr)
	Just port -> do
		handle <- initTCPServer port
		return (handle, handle, handle)

-- TODO: check for invalid commands - for now, it just exits, if an invalid command name is used!
startApp (stdIn, stdOut, stdErr) appAndParams = do
	let command = foldl1 concWithSpace appAndParams
	putStrLn $ "running \"" ++ command ++ "\"..."
 	runProcess
		(head appAndParams)
		(tail appAndParams)
		Nothing Nothing
		(Just stdIn) (Just stdOut) (Just stdErr)
	where
		concWithSpace l r = l ++ " " ++ r

initTCPServer tcpPort = withSocketsDo $ do
	sock <- listenOn $ PortNumber $ fromIntegral tcpPort
	(handle, _, _) <- accept sock
	hSetBuffering handle NoBuffering
	return handle
