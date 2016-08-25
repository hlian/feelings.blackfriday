{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent.Timer (repeatedTimer)
import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified System.Directory as System
import qualified System.IO as System
import qualified System.IO.Temp as System

import           Control.Concurrent.STM.TMVar
import           Data.Aeson
import           Feelings.Batteries
import           Feelings.Types
import           Lucid
import           Snap.Core hiding (path)
import           Snap.Http.Server

main :: IO ()
main = do
  feelings <- initialFeelingsM
  _ <- repeatedTimer (serialize feelings) (sDelay 1)
  httpServe config (site feelings)
  where
    config = setPort 4445 mempty

initialFeelingsM :: IO (TMVar [Feeling])
initialFeelingsM = do
  bytes_ <- Bytes.readFile "feelings.txt"
  feelings <-
    case bytes_ ^. (lazy . to eitherDecode) of
      Left _ ->
        return []
      Right alist ->
        return alist
  getCurrentDirectory >>= print
  length feelings `seq` return ()
  newTMVarIO feelings

serialize :: TMVar [Feeling] -> IO ()
serialize feelingsM = do
  feelings <- atomically (readTMVar feelingsM)
  System.createDirectoryIfMissing True "tmp"
  System.withTempFile "tmp" "feelings."
    (\f h -> do
      Bytes.hPutStr h (encode feelings ^. strict)
      System.hFlush h
      hClose h
      System.renameFile f "feelings.txt")

site :: TMVar [Feeling] -> Snap ()
site feelingsM = do
  modifyResponse (setContentType "text/html")
  dir "static" (serveDirectory ".") <|> route [
    ("", home feelingsM)
    , ("/feeling", feeling feelingsM)
    , ("/sms", sms feelingsM)]

sms :: TMVar [Feeling] -> Snap ()
sms feelingsM = do
  time <- liftIO getCurrentTime
  inputs <- sequence (seek <$> ["Body", "From", "To"])
  case inputs of
    [Just rawText, Just rawFrom, Just rawTo] -> do
      liftIO . atomically . void $ do
        let afeeling = Feeling time rawText (ViaPhone (Phone rawFrom))
        old <- readTMVar feelingsM
        swapTMVar feelingsM (afeeling : old)
      writeText (appEndo (fill "?from" rawTo <> fill "?to" rawFrom) "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response><Sms from=\"?from\" to=\"?to\">thank you my child</Sms></Response>")
    _ -> do
      let e = "bad (?Body, ?From) parameters: " <> present inputs
      modifyResponse (setResponseStatus 400 (utf8 # e))
  where
    fill k v = Endo (Text.replace k v)

lucid :: Html a -> Snap ()
lucid = writeText . view strict . renderText

-- | How does this function not already exist
seek :: MonadSnap m => Bytes.ByteString -> m (Maybe Text)
seek key = do
  req <- getRequest
  let raw = rqPostParam key req
  return (raw ^? _Just . ix 0 . utf8)

feeling :: TMVar [Feeling] -> Snap ()
feeling feelingsM = do
  req <- getRequest
  time <- liftIO getCurrentTime
  let rawText = rqPostParam "text" req
  case rawText ^? (_Just . ix 0 . utf8) of
    Just text_ -> do
      liftIO . atomically . void $ do
        let afeeling = Feeling time text_ ViaWeb
        old <- readTMVar feelingsM
        swapTMVar feelingsM (afeeling : old)
      redirect "/"
    Nothing -> do
      let e = "bad ?text parameter: " <> present rawText
      modifyResponse (setResponseStatus 400 (utf8 # e))

home :: TMVar [Feeling] -> Snap ()
home feelingsM = do
  isFriday <- isTimeFriday <$> liftIO getCurrentTime
  feelings <- liftIO . atomically $ readTMVar feelingsM
  lucid (html' isFriday feelings)
  where
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
    feeling' (Feeling t i via) = do
      let classes = [ if isTimeFriday t then "friday" else "timeslip"
                    , case via of ViaPhone _ -> "phone"; _ -> ""
                    ]
      with p_ [class_ . comma $ classes] (toHtml i)

    good :: Bool -> Html ()
    good isFriday = do
      p_ (if isFriday then "it's friday in nyc" else "it's not friday in nyc")
      p_ "feelings submitted on a non-friday will be gray-punished"
      p_ (mconcat [ "fun facts"
                  , " // html is dumb sms is cool and hip and smart " <> a_ [href_ "tel://1631400FEEL"] "+1631400FEEL"
                  , " // hyper-draw feeling to make feeling big and color " <> a_ [href_ "https://sky-carver.hyperdev.space"] (i_ "Frog Feels")
                  ]
         )
      with form_ [ action_ "/feeling"
                 , method_ "post"
                 ]
        (p_ (textarea_ [placeholder_ "type a feeling", name_ "text", tabindex_ "1"] "") <>
         p_ (input_ [type_ "submit", tabindex_ "2"]))

comma :: [Text] -> Text
comma =
  Text.intercalate " " . filter (not . Text.null)
