module Model.Irc where

import System.IO
import System.Directory
import Control.Monad.Trans (liftIO, MonadIO)
import App.Types
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as Char8

freenode = ( mkDefaultConfig "chat.freenode.net" "SimpleFlippy" ) { cChannels = ["#flipstonesandbox"] }

joinChannel :: MonadIO m => m ()
joinChannel = liftIO $ do
       nameservIn <- openFile "/tmp/irc/chat.freenode.net/in" WriteMode
       hPutStrLn nameservIn "/j #flipstonesandbox"
       hFlush nameservIn
       hClose nameservIn

writeToChat :: MonadIO m => String -> m ()
writeToChat s = liftIO $ do
   joinChannel
   channelIn <- openFile "/tmp/irc/chat.freenode.net/#flipstonesandbox/in" WriteMode
   hPutStrLn channelIn s
   hFlush channelIn
   hClose channelIn


simpleWriteToChat :: MonadIO m => String -> m ()
simpleWriteToChat message = liftIO $ do
  instanceEither <- connect freenode False True
  let Right ircInstance = instanceEither
  let bytes = Char8.pack message
  sendMsg ircInstance "#flipstonesandbox" bytes
  disconnect ircInstance "QUIT"