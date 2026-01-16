-- / Library modules containing all social network commands
module Lib
    ( User(..)
    , Message(..)
    , createUsers
    ) where

import System.Random

-- | Represents a user in the social network
-- Contains username and unique userID
data User = User
    { username :: String    -- Users name 
    , userId :: Int      -- Unique user identifier
    , isOnline :: Bool   -- User's online status
    } deriving (Show, Eq)

-- | Represents a message between users
data Message = Message
    { sender :: String  -- Sender's username
    , receiver :: String  -- Receiver's username
    , messageContent :: String  -- Content of the message
    } deriving (Show)

-- | Create n users with random offline/online status 50/50
createUsers :: Int -> IO [User]
createUsers n = mapM createUser [1..n]
    where
        createUser i = do
            online <- randomIO :: IO Bool -- True/False
            return $ User ("User " ++ show i) i online
