{-# LANGUAGE CPP #-}
module Data.Streaming.Network.Internal
    ( ServerSettings (..)
    , ClientSettings (..)
    , HostPreference (..)
    , Message (..)
#if !WINDOWS
    , ServerSettingsUnix (..)
    , ClientSettingsUnix (..)
#endif
    ) where

import Data.String (IsString (..))
import Data.ByteString (ByteString)
import Network.Socket (Socket, SockAddr)

-- | Settings for a TCP server. It takes a port to listen on, and an optional
-- hostname to bind to.
data ServerSettings m = ServerSettings
    { serverPort :: Int
    , serverHost :: HostPreference
    , serverAfterBind :: Socket -> m ()
    , serverNeedLocalAddr :: Bool
    }

-- | Settings for a TCP client, specifying how to connect to the server.
data ClientSettings = ClientSettings
    { clientPort :: Int
    , clientHost :: ByteString
    }

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @!4@ means @HostIPv4Only@
--
-- * @*6@ means @HostIPv6@
--
-- * @!6@ means @HostIPv6Only@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv4Only
  | HostIPv6
  | HostIPv6Only
  | Host String
    deriving (Eq, Ord, Show, Read)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "!4" = HostIPv4Only
    fromString "*6" = HostIPv6
    fromString "!6" = HostIPv6Only
    -}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s'@('!':s) =
        case s of
            ['4'] -> HostIPv4Only
            ['6'] -> HostIPv6Only
            _ -> Host s'
    fromString s = Host s

-- | Settings for a Unix domain sockets server.
data ServerSettingsUnix m = ServerSettingsUnix
    { serverPath :: FilePath
    , serverAfterBindUnix :: Socket -> m ()
    }

-- | Settings for a Unix domain sockets client.
data ClientSettingsUnix = ClientSettingsUnix
    { clientPath :: FilePath
    }

-- | Representation of a single UDP message
data Message = Message { msgData :: {-# UNPACK #-} !ByteString
                       , msgSender :: !SockAddr
                       }
