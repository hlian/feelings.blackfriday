{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import           Control.Concurrent.Suspend.Lifted (sDelay)
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import           Data.Time.Clock (getCurrentTime, UTCTime)
import           Data.Time.LocalTime (localDay)
import           Snap.Util.FileServe (serveDirectory)
import           System.Directory (getCurrentDirectory)


import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Timer as D4
import qualified Data.ByteString as Bytes
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZ
import qualified System.Directory as System
import qualified System.IO as System
import qualified System.IO.Temp as System

import           BasePrelude hiding (lazy)
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Lucid
import           Snap.Core hiding (path)
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
  _ <- D4.repeatedTimer (serialize feelings) (sDelay 1)
  httpServe config (site feelings)
  where
    config = setPort 4445 mempty

initialFeelingsM :: IO (M.MVar [Feeling])
initialFeelingsM = do
  feelings <- (fromJust . decode . (^. lazy)) <$> Bytes.readFile "feelings.txt"
  getCurrentDirectory >>= print
  length feelings `seq` return ()
  M.newMVar feelings

serialize :: MVar [Feeling] -> IO ()
serialize feelingsM = do
  feelings <- M.readMVar feelingsM
  System.withTempFile "tmp" "feelings."
    (\f h -> do
      Bytes.hPutStr h (encode feelings ^. strict)
      System.hFlush h
      hClose h
      System.renameFile f "feelings.txt")

site :: M.MVar [Feeling] -> Snap ()
site feelingsM = do
  modifyResponse (setContentType "text/html")
  dir "static" (serveDirectory ".") <|> route [("", home feelingsM), ("/feeling", feeling feelingsM)]

lucid :: Html a -> Snap ()
lucid = writeText . TL.toStrict . renderText

present :: (Show a) => a -> Text
present = T.pack . show

dayOfWeek :: Day -> Int
dayOfWeek = snd . sundayStartWeek

isTimeFriday :: UTCTime -> Bool
isTimeFriday = (== 5) . dayOfWeek . localDay . TZ.utcToLocalTimeTZ (TZ.tzByLabel TZ.America__New_York)

feeling :: MVar [Feeling] -> Snap ()
feeling feelingsM = do
  req <- getRequest
  time <- liftIO getCurrentTime
  let text = (decodeUtf8 . head . fromJust) (rqPostParam "text" req)
  liftIO $ M.modifyMVar_ feelingsM (return . (:) (Feeling time text))
  redirect "/"

home :: MVar [Feeling] -> Snap ()
home feelingsM = do
  isFriday <- isTimeFriday <$> liftIO getCurrentTime
  toLucid <- liftIO $ M.modifyMVar feelingsM (htmlM isFriday)
  lucid toLucid
  where
    htmlM isFriday feelings =
      return (feelings, html' isFriday feelings)

    html' :: Bool -> [Feeling] -> Html ()
    html' isFriday feelings = do
      doctype_
      html_ (head' <> body' isFriday feelings)

    head' =
      head_ (title_ "feelings.blackfriday" <>
             css "static/css/css.css.css" <>
             meta_ [charset_ "utf-8"])
    css path =
      link_ [rel_ "stylesheet", href_ path]

    body' :: Bool -> [Feeling] -> Html ()
    body' isFriday feelings = body_ $ do
      h1_ "feelings.blackfriday"
      good isFriday
      h2_ "feelings"
      forM_ feelings feeling'

    feeling' :: Feeling -> Html ()
    feeling' (Feeling t i) =
      with p_ [class_ (if isTimeFriday t then "friday" else "timeslip")] (toHtml i)

    good :: Bool -> Html ()
    good isFriday = do
      p_ (if isFriday then "it's friday in nyc" else "it's not friday in nyc")
      p_ "feelings submitted on a non-friday will be gray-punished"
      with form_ [ action_ "/feeling"
                 , method_ "post"
                 ]
        (p_ (textarea_ [placeholder_ "type a feeling", name_ "text", tabindex_ "1"] "") <>
         p_ (input_ [type_ "submit", tabindex_ "2"]))
