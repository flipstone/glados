{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App.Types where

import Control.Applicative (Applicative)
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class  (MonadIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, MonadLogger(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Postgresql
import Happstack.Server

type AppBackend = SqlPersistT (LoggingT (ResourceT IO))

class BackendHost m where
  runDB :: AppBackend a -> m a

newtype App a = App (ServerPartT AppBackend a)
  deriving ( ServerMonad
           , Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadPlus
           )

runBackendPool :: ConnectionPool -> AppBackend a -> IO a
runBackendPool pool backend = runResourceT
                            $ runStdoutLoggingT
                            $ runSqlPool backend pool

runApp :: ConnectionPool -> App a -> ServerPartT IO a
runApp pool (App action) = mapServerPartT (runBackendPool pool)
                                          action

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

instance BackendHost App where
  runDB a = App (lift a)


