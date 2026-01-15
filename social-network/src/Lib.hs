-- / Library modules containing all social network commands
module Lib
    ( User(..)
    , Message(..)
    , createUsers
    ) where

-- / Represents a user in the social network
-- Contains username and unique userID
data User = User
    { username :: String    -- ^ Users name 
    , userId :: Int      -- ^ Unique user identifier
    } deriving (Show, Eq)

-- /  Represents a message between users
data Message = Message
    { sender :: String  -- ^ Sender's username
    , receiver :: String  -- ^ Receiver's username
    , messageContent :: String  -- ^ Content of the message
    } deriving (Show)

-- / Creates a list of n users
-- "User1, User2 ..."
createUsers :: Int -> [User]
createUsers n = [User ("User " ++ show i) i | i <- [1..n]] -- List comprehension to n number of users