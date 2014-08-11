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
import qualified Data.Vector as V
import Numeric.FFT
import Data.Complex

dt = 0.001
tmax = 1

per = 300
amp = 3
sinf t = amp * sin (per*t/(2*pi))

ts, sins :: [Double]

ts = map (*dt) [0..(tmax/dt)]

sins = map sinf ts

main = do
  wf <- sampleIO $ mapM (`normal` 0.5) sins
  let v = V.fromList $ map (:+0) wf
  fd <- fft v
  let psd = V.map (logBase 10) $ halveFirstAndLast $ V.map ((*2) . (^2) . magnitude) $ firstHalf fd
  T.writeFile "/tmp/bigo.html" $ inRadian $ do
         scatterPlot 400 600 "sins" $ zip ts wf
         scatterPlot 400 600 "psd" $ zip [0..] $ V.toList psd
  putStrLn "hello world"

firstHalf v = V.take ((V.length v `div` 2) +1 ) v

halveFirstAndLast v = let last  = V.length v-1 
                      in v V.// [(0, (v V.! 0)/2), (last, (v V.! last)/2)]


inRadian :: H.Html -> T.Text
inRadian conts = TL.toStrict $ renderHtml $ H.docTypeHtml $ do
      H.head $ radianHeaders
      H.body ! ngApp "dummy" $ do
          _ <- conts
          radianScripts
          radianDummyModule "dummy"
