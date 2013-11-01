module IRC.Types where

type Nickname = String
type Username = String
type Realname = String
type Channel  = String
type Hostname = String
type Password = String
type Target   = String
type Key      = String
type Mode     = String
type Command  = String
type Args     = [String]

data Prefix   = Server Hostname
              | User Nickname (Maybe Username) (Maybe Hostname) deriving Show
data Message  = Message (Maybe Prefix) Command Args deriving Show

data Privmsg  = Privmsg Nickname Target String deriving Show

