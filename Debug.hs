module Debug (log) where

import Control.Monad.IO.Class
import Prelude                hiding (log)
import System.IO
import System.IO.Unsafe

logfile :: Handle
logfile = unsafePerformIO $ do
  file <- openFile "/home/mitchell/.xmonad-log" AppendMode
  hSetBuffering file NoBuffering
  hSetEncoding file utf8
  pure file
{-# NOINLINE logfile #-}

log :: MonadIO m => String -> m ()
log = liftIO . hPutStrLn logfile
