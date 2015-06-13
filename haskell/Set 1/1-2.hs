import System.Environment
import Crypto

main = do
    args <- getArgs
    (buffer1, buffer2) <- case args of
        [] -> do
            putStrLn "Enter the first hex string: "
            buffer1 <- getLine
            putStrLn "Enter the second hex string: "
            buffer2 <- getLine
            return (buffer1, buffer2)
        [buffer1] -> error "Expected 2 inputs, received 1"
        buffer1 : buffer2 : xs -> return (buffer1, buffer2)
    putStrLn (xorHexes buffer1 buffer2)