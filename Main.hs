module Main where

import ParseArgs

--import Prelude hiding( Left, Right, Either)
import System.Environment
import Network


main = do
	args <- getArgs
	let settings = parseArgs args
	case settings of
		Left error -> fail $ "error while reading options: \n" ++ error
		Right settings -> putStr $ "parameters parsed: " ++ show settings
	putStrLn "starting server..."
	serve settings
	return ()

serve settings = return ()

