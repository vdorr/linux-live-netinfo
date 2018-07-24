{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}

import System.Console.ANSI (clearScreen)

import System.Linux.NetInfo

main :: IO ()
main = withNetInfo $ \netInfoSock ->
	newsLoop netInfoSock $ \(event, ifMap) -> do
		clearScreen
		putStrLn "--------------------------------------------------------------------------------"
		dumpIfMap putStrLn ifMap
		return Nothing

