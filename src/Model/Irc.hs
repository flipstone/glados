module Model.Irc where

import System.IO
import System.Directory
import Control.Monad.Trans (liftIO, MonadIO)
import App.Types

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
