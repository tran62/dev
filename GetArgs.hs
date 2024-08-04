import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are: "
    mapM putStrLn args
    putStrLn "The programe name is : "
    putStrLn progName