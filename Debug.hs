module Debug
( unimplemented
, debug
, logged
) where

unimplemented = error "unimplemented"

debug  msg = putStrLn $ "[>] " ++ msg
logged msg = putStrLn $ "[*] " ++ msg
