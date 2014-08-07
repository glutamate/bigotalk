{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Main where

import Math.Probably.Types
import Math.Probably.Sampler
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H hiding (form, label)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import  Text.Blaze.Html.Radian
import  Text.Blaze.Html.AngularJS
dt = 0.001
tmax = 1

per = 175
amp = 3
sinf t = amp * sin (per*t/(2*pi))

ts, sins :: [Double]

ts = map (*dt) [0..(tmax/dt)]

sins = map sinf ts

main = do
  T.writeFile "/tmp/bigo.html" $ inRadian $ scatterPlot 400 600 "sins" $ zip ts sins
  putStrLn "hello world"


inRadian :: H.Html -> T.Text
inRadian conts = TL.toStrict $ renderHtml $ H.docTypeHtml $ do
      H.head $ radianHeaders
      H.body ! ngApp "dummy" $ do
          _ <- conts
          radianScripts
          radianDummyModule "dummy"
