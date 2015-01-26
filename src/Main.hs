{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import           Control.Concurrent.Suspend.Lifted (sDelay)
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import           Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import           Snap.Core hiding (path)
import           Snap.Util.FileServe (serveDirectory)

import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Timer as D4
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           BasePrelude
import           Data.Aeson
import           Lucid
import           Snap.Http.Server.Config
import           Snap.Http.Server

data Feeling = Feeling UTCTime Text deriving (Show, Eq)

instance FromJSON Feeling where
  parseJSON (Object o) =
    Feeling <$> o .: "time"
            <*> o .: "text"
  parseJSON _ = error "FromJSON Feeling: expecting object"

instance ToJSON Feeling where
  toJSON (Feeling time text) =
    object ["time" .= time, "text" .= text]

main :: IO ()
main = do
  feelings <- initialFeelingsM
  D4.repeatedTimer (serialize feelings) (sDelay 1)
  httpServe config (site feelings)
  where
    config = setPort 4445 mempty

initialFeelingsM :: IO (M.MVar [Feeling])
initialFeelingsM = do
  feelings <- (fromJust . decode) <$> BSL.readFile "feelings.txt"
  length feelings `seq` return ()
  M.newMVar feelings

serialize :: MVar [Feeling] -> IO ()
serialize feelingsM = do
  feelings <- M.readMVar feelingsM
  BSL.writeFile "feelings.txt" (encode feelings)

site :: M.MVar [Feeling] -> Snap ()
site feelingsM = do
  modifyResponse (setContentType "text/html")
  dir "static" (serveDirectory ".") <|> route [("", home feelingsM), ("/feeling", feeling feelingsM)]

lucid :: Html a -> Snap ()
lucid = writeText . TL.toStrict . renderText

present :: (Show a) => a -> Text
present = T.pack . show

dayOfWeek :: UTCTime -> Int
dayOfWeek = snd . sundayStartWeek . utctDay

isFriday :: UTCTime -> Bool
isFriday = (== 5) . dayOfWeek

feeling :: MVar [Feeling] -> Snap ()
feeling feelingsM = do
  req <- getRequest
  time <- liftIO getCurrentTime
  let text = (decodeUtf8 . head . fromJust) (rqPostParam "text" req)
  liftIO $ M.modifyMVar_ feelingsM (return . (:) (Feeling time text))
  redirect "/"

home :: MVar [Feeling] -> Snap ()
home feelingsM = do
  day <- dayOfWeek <$> liftIO getCurrentTime
  toLucid <- liftIO $ M.modifyMVar feelingsM (htmlM day)
  lucid toLucid
  where
    htmlM day feelings =
      return (feelings, html' day feelings)

    html' :: Int -> [Feeling] -> Html ()
    html' day feelings = do
      doctype_
      html_ (head' <> body' day feelings)

    head' =
      head_ (title_ "feelings.blackfriday" <>
             css "static/css/css.css.css" <>
             meta_ [charset_ "utf-8"])
    css path =
      link_ [rel_ "stylesheet", href_ path]

    body' :: Int -> [Feeling] -> Html ()
    body' day feelings = body_ $ do
      h1_ "feelings.blackfriday"
      if day == 5 || cheating then good else bad
      h2_ "feelings"
      forM_ feelings feeling'

    feeling' :: Feeling -> Html ()
    feeling' (Feeling t i) =
      with p_ [class_ (if isFriday t then "friday" else "timeslip")] (toHtml i)

    good = do
      p_ "it's friday"
      with form_ [ action_ "/feeling"
                 , method_ "post"
                 ]
        (p_ (textarea_ [placeholder_ "type a feeling", name_ "text", tabindex_ "1"] "") <>
         p_ (input_ [type_ "submit", tabindex_ "2"]))
    bad =
      p_ "come back when it's friday"
    cheating =
      True
