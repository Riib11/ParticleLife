module Debug
( debug
, logged
) where

debug  msg = putStrLn $ "[>] " ++ msg
logged msg = putStrLn $ "[*] " ++ msg
