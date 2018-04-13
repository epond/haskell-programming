module Timer where

import Control.Concurrent

-- printTimer prints a message repeatedly with a fixed delay each time
printTimer :: IO ()
printTimer = foldr delay (return ()) $ map (\_ -> doWork) [1..]

delay :: IO () -> IO () -> IO ()
delay newjob alljobs = do
    newjob
    threadDelay 1000000
    alljobs

doWork :: IO ()
doWork = putStrLn "doing work"