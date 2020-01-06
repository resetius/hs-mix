module Lib
    ( someFunc
    ) where

import Mix.Types


test :: Byte
test = 63 + 1
    where a = Byte 0

someFunc :: IO ()
someFunc = putStrLn $ show test
