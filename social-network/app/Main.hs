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

    putStrLn "Setup completed!"

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
    idx <- randomRIO (0, length otherUsers - 1)
    return $ otherUsers !! idx

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
            -- Continue to send messages
            putMVar messageCounter count

            -- Initialise random delay (simulate a texting chain)
            delay <- randomRIO (100000, 1000000)
            threadDelay delay

            -- Select a random user to send a message to
            receiver <- getRandomUser currentUser allUsers

            -- Get random message
            msg <- getRandomMessage

            -- return the message
            putStrLn $ username currentUser ++ " -> " ++ username receiver ++ ": " ++ msg

            -- increment to the message counter
            _ <- takeMVar messageCounter
            putMVar messageCounter (count + 1)

            -- increment the receiver's message count
            let receiverIdx = userId receiver - 1
            oldCount <- takeMVar (userCounter !! receiverIdx)
            putMVar (userCounter !! receiverIdx) (oldCount + 1)

            -- recursively call the userThread to continue sending messages
            userThread currentUser allUsers messageCounter userCounter