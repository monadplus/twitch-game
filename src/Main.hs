{-# LANGUAGE OverloadedStrings          #-}
module Main where

------------------------------------------------------------

import           API.Lib
import           API.Types
import           Control.Concurrent.Async
import           Game.Lib

------------------------------------------------------------

main :: IO ()
main = do
  ctx <- newServerState
  let port = 8080
  putStrLn ("Starting server on port " <> show port)
  concurrently_
    (runServer port ctx)
    (runGame ctx)
