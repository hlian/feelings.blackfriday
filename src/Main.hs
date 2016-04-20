{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Timer as D4
import qualified Data.ByteString as Bytes
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZ
import qualified System.Directory as System
import qualified System.IO as System
import qualified System.IO.Temp as System

import           Batteries
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
  toJSON (Feeling time text_) =
    object ["time" .= time, "text" .= text_]

main :: IO ()
main = do
  feelings <- initialFeelingsM
  _ <- D4.repeatedTimer (serialize feelings) (sDelay 1)
  httpServe config (site feelings)
  where
    config = setPort 4445 mempty

initialFeelingsM :: IO (M.MVar [Feeling])
initialFeelingsM = do
  feelings <- (fromJust . decode . view lazy) <$> Bytes.readFile "feelings.txt"
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
lucid = writeText . view strict . renderText

present :: (Show a) => a -> Text
present = view packed . show

dayOfWeek :: Day -> Int
dayOfWeek = snd . sundayStartWeek

isTimeFriday :: UTCTime -> Bool
isTimeFriday = (== 5) . dayOfWeek . localDay . TZ.utcToLocalTimeTZ (TZ.tzByLabel TZ.America__New_York)

feeling :: MVar [Feeling] -> Snap ()
feeling feelingsM = do
  req <- getRequest
  time <- liftIO getCurrentTime
  let rawText = rqPostParam "text" req
  case rawText  ^? (_Just . ix 0 . utf8) of
    Just text_ -> do
      liftIO $ M.modifyMVar_ feelingsM (return . (:) (Feeling time text_))
      redirect "/"
    Nothing -> do
      let e = "bad ?text parameter: " <> present rawText
      modifyResponse (setResponseStatus 400 (utf8 # e))

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
