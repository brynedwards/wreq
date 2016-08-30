module Network.Wreq.Internal.OAuth1 (signRequest) where

import Network.HTTP.Client (Request(..))
import Web.Authenticate.OAuth ( signOAuth, newOAuth, oauthConsumerKey
                              , oauthConsumerSecret, newCredential
                              , emptyCredential)
import qualified Data.ByteString as S

signRequest :: S.ByteString -> S.ByteString -> Maybe S.ByteString -> Maybe S.ByteString -> Request -> IO Request
signRequest consumerToken consumerSecret token tokenSecret = signOAuth app (creds token tokenSecret)
  where
    app = newOAuth { oauthConsumerKey = consumerToken, oauthConsumerSecret = consumerSecret }
    creds (Just token') (Just tokenSecret') = newCredential token' tokenSecret'
    creds _ _                               = emptyCredential
