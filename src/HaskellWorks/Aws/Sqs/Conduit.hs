module HaskellWorks.Aws.Sqs.Conduit
  ( sqsSink
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans            (lift)
import Data.Conduit
import Data.Text
import Network.AWS                    hiding (await, response)
import Network.AWS.SQS.ReceiveMessage
import Network.AWS.SQS.SendMessage

import qualified Network.AWS as AWS

sqsSink :: MonadAWS m => SendMessage -> Conduit Text m SendMessageResponse
sqsSink sendMessage = do
  maybeM <- await
  case maybeM of
    Just m -> do
      resp <- lift $ send sendMessage
      yield resp
      sqsSink sendMessage
    Nothing -> return ()

sqsSource :: MonadAWS m => ReceiveMessage -> Source m ReceiveMessageResponse
sqsSource receiveMessage = do
  m <- lift $ send receiveMessage
  yield m
  sqsSource receiveMessage
