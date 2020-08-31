import Cmdline

main :: IO ()
main = do
    opts <- parseCmdlineArgs
    putStrLn (show opts)
