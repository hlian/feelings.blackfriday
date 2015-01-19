{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import           BasePrelude
import qualified Control.Concurrent.MVar as M
import           Control.Monad.Trans (liftIO)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (getCurrentTime, utctDay, UTCTime)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Calendar.OrdinalDate
import           Lucid
import           Snap.Core (Snap, route, dir, writeText, getRequest, rqPostParam, rqPostParams, redirect)
import           Snap.Util.FileServe
import           Snap.Http.Server

data Feeling = Feeling Text deriving (Show, Eq)

main :: IO ()
main = do
  feelings <- liftIO (M.newMVar [ Feeling "i like you"
                                , Feeling "i don't like you"
                                , Feeling "i like you more"])
  quickHttpServe (site feelings)

site :: M.MVar [Feeling] -> Snap ()
site feelingsM = do
  dir "static" (serveDirectory ".") <|> route [("", home feelingsM), ("/feeling", feeling feelingsM)]

lucid :: Html a -> Snap ()
lucid = writeText . TL.toStrict . renderText

present :: (Show a) => a -> Text
present = T.pack . show

dayOfWeek :: UTCTime -> Int
dayOfWeek = snd . sundayStartWeek . utctDay

feeling :: MVar [Feeling] -> Snap ()
feeling feelingsM = do
  req <- getRequest
  let text = (decodeUtf8 . head . fromJust) (rqPostParam "text" req)
  liftIO $ M.modifyMVar_ feelingsM (htmlM text)
  redirect "/"
  where
    htmlM text feelings = do
      return (Feeling text : feelings)

home :: MVar [Feeling] -> Snap ()
home feelingsM = do
  day <- dayOfWeek <$> liftIO getCurrentTime
  toLucid <- liftIO $ M.modifyMVar feelingsM (htmlM day)
  lucid toLucid
  where
    htmlM day feelings = do
      putStrLn (show feelings)
      return (feelings, html' day feelings)

    html' :: Int -> [Feeling] -> Html ()
    html' day feelings = do
      doctype_
      html_ (head' <> body' day feelings)

    head' = do
      head_ (title_ "feelings.blackfriday" <>
             css "static/css/css.css.css" <>
             meta_ [charset_ "utf-8"])
    css path =
      link_ [rel_ "stylesheet", href_ path]

    body' :: Int -> [Feeling] -> Html ()
    body' day feelings = do
      h1_ "feelings.blackfriday"
      if day == 5 || cheating then good else bad
      h2_ "feelings"
      forM_ feelings feeling'

    feeling' :: Feeling -> Html ()
    feeling' (Feeling i) = p_ (toHtml i)

    good = do
      p_ "it's friday"
      with form_ [ action_ "/feeling"
                 , method_ "post"
                 ]
        (p_ (textarea_ [placeholder_ "type a feeling", name_ "text"] "") <>
         p_ (input_ [type_ "submit"]))
    bad = do
      p_ "come back when it's friday"
    cheating =
      True
