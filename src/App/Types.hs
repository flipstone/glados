{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module App.Types where

import Control.Applicative (Applicative, Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class  (MonadIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, MonadLogger(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Reader
import Database.Persist.Postgresql
import Happstack.Server

type AppBackend = SqlPersistT (LoggingT (ResourceT IO))

data ChatConfig = ChatConfig {
  serverHandle :: String
}

class BackendHost m where
  runDB :: AppBackend a -> m a

newtype App a = App (ServerPartT (ReaderT ChatConfig AppBackend) a)
  deriving ( ServerMonad
           , Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadPlus
           )

instance Alternative App where
  empty = mzero
  (<|>) = mplus


runBackendPool :: ConnectionPool -> AppBackend a -> IO a
runBackendPool pool backend = runResourceT
                            $ runStdoutLoggingT
                            $ runSqlPool backend pool

runApp :: ConnectionPool -> App a -> ServerPartT IO a
runApp pool (App action) = mapServerPartT (initializeIrc pool)
                                          action

initializeIrc :: ConnectionPool -> (ReaderT ChatConfig AppBackend) a -> IO a
initializeIrc pool readerWrappedBackend =
  runBackendPool pool (runReaderT readerWrappedBackend (ChatConfig "foo"))

instance FilterMonad Response App where
  setFilter = App . setFilter
  composeFilter = App . composeFilter
  getFilter (App serverPart) = App (getFilter serverPart)

instance WebMonad Response App where
  finishWith = App . finishWith

instance HasRqData App where
  askRqEnv = App askRqEnv
  localRqEnv f (App action) = App (localRqEnv f action)
  rqDataError = App . rqDataError

instance MonadSqlPersist App where
  askSqlConn = App (lift askSqlConn)

instance MonadLogger App where
  monadLoggerLog loc source level msg =
    App (lift $ monadLoggerLog loc source level msg)

instance MonadReader ChatConfig App where
  ask = App ask
  local f (App action) = App (local f action)

instance BackendHost App where
  runDB a = App (lift (lift a))
