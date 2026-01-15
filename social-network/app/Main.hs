module Main where

import Lib
import Control.Concurrent
import System.Random

-- / Main entry point of the social network application
main :: IO ()
main = do
    putStrLn "Welcome to the Somaans' Social Network Platform!"
    putStrLn "Creating users..."

    let users = createUsers 10 -- Create 10 users

    -- Print the users
    mapM_ print users
    putStrLn "\n Initialising..."

    -- Creating MVar to count total messages sent
    messageCounter <- newMVar 0

    -- Create MVar for each user to track their received messages
    -- One MVar per user, count starts at 0
    userCounter <- mapM (\_ -> newMVar 0) users

    -- Start a thread for each user
    mapM_ (\user -> forkIO $ userThread user users messageCounter userCounter) users

    -- wait until 100 messages have been sent
    completionStatus messageCounter

    putStrLn "Statistics:"
    printStatistics users userCounter

-- / Generates random message content 
getRandomMessage :: IO String
getRandomMessage = do
    r <- randomRIO (0, 5) :: IO Int
    return $ messages !! r
    where
        messages = ["Hi", "Morning", "How are you?", "What are you up to?", "Coffee?", "Fancy the boozer later?"]

-- / Selects a user at random from the list
getRandomUser :: User -> [User] -> IO User
getRandomUser current allUsers = do
    let otherUsers = filter (/= current) allUsers
    randomIndex <- randomRIO (0, length otherUsers - 1)
    return $ otherUsers !! randomIndex

-- / Threading behaviour, runs a loop sending messages
userThread :: User -> [User] -> MVar Int -> [MVar Int] -> IO ()
userThread currentUser allUsers messageCounter userCounter = do
    -- checker to see if 100 messages have been sent
    count <- takeMVar messageCounter

    if count >= 100
        then do
            -- End condition met, output count and exit
            putMVar messageCounter count
            return()
        else do
            -- Random delay between messages
            delay <- randomRIO (100000, 1000000)
            threadDelay delay

            -- Select a random user to send a message to
            receiver <- getRandomUser currentUser allUsers

            -- Get random message
            msg <- getRandomMessage

            -- example first message output
            if count == 0
                then putStrLn $ "\nExample Message: " ++ username currentUser ++ " to " ++ username receiver ++ ": " ++ msg ++ "\n"
                --print progress every 25 messages to clear output
                else if (count + 1) `mod` 25 == 0
                    then putStrLn $ "Progress: " ++ show (count + 1) ++ " messages sent!"
                    else return()

            -- Continue to send messages
            putMVar messageCounter (count + 1)

            -- increment the receiver's message count
            let receiverIdx = userId receiver - 1
            oldCount <- takeMVar (userCounter !! receiverIdx)
            putMVar (userCounter !! receiverIdx) (oldCount + 1)

            -- recursively call the userThread to continue sending messages
            userThread currentUser allUsers messageCounter userCounter

-- / Waits until 100 messages have been sent    
completionStatus :: MVar Int -> IO ()
completionStatus messageCounter = do
    count <- takeMVar messageCounter
    putMVar messageCounter count
    if count >= 100
        then do
            putStrLn "\n100 messages have been sent!\n"
            threadDelay 1000000 -- wait a second to allow final messages to print
            return()
        else do
            threadDelay 500000 -- wait half a second before checking again
            completionStatus messageCounter

-- / Prints statistics of messages received by each user
printStatistics :: [User] -> [MVar Int] -> IO ()
printStatistics users counters = do
    counts <- mapM takeMVar counters  -- Get counts from each MVar
    let stats = zip users counts    -- Combine users with their message counts
    mapM_ (\(user, count) -> putStrLn $ username user ++ " received " ++ show count ++ " messages.") stats