module Main where

import ProgramTypes
import ParseArgs

import System.Environment
import Network
import System.IO


main = do
	args <- getArgs
	let settings = parseArgs args
	case settings of
		Left error -> fail $ "error while reading options: \n" ++ error
		Right settings -> do
			putStrLn "starting server..."
			ioSettings <- initIOSettings settings
			serve ioSettings
	return ()

serve IOSettings{ get=get, put=put } = do
	put "Hallo"
	return ()

initIOSettings settings = do
	case port settings of
		Nothing -> return $ IOSettings {
			get = getLine,
			put = putStrLn
		}
		Just port -> do
			handle <- initTCPServer port
			return $ IOSettings {
				get = hGetLine handle,
				put = hPutStrLn handle
			}
initTCPServer port = withSocketsDo $ do
	sock <- listenOn $ PortNumber $ fromIntegral port
	(handle, _, _) <- accept sock
	hSetBuffering handle NoBuffering
	return handle

data IOSettings = IOSettings {
	get :: IO String,
	put :: String -> IO ()
}
