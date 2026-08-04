module Agent.ReadOnly
  ( ReadOnlyConn
  , withReadOnlyConn
  , receive
  ) where

import Data.ByteString.Lazy qualified as BSL
import Network.WebSockets qualified as WS

-- | A WebSocket connection that exposes only @receive@.
-- There is no @send@ or @close@ function on this type -
-- the type system enforces that the CLI agent is read-only.
newtype ReadOnlyConn = ReadOnlyConn WS.Connection

-- | Open a read-only WebSocket connection to the given host/port/path.
withReadOnlyConn :: String -> Int -> String -> (ReadOnlyConn -> IO a) -> IO a
withReadOnlyConn host port path action =
  WS.runClient host port path (action . ReadOnlyConn)

-- | Receive the next binary message from the head.
receive :: ReadOnlyConn -> IO BSL.ByteString
receive (ReadOnlyConn conn) = WS.receiveData conn
