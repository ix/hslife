{-# LANGUAGE OverloadedStrings #-}
module Main where

import Logic.Life
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Data.Array (elems)
import Foreign.C.Types
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock.POSIX
import qualified SDL

screenScale = 400 
white = SDL.V4 255 255 255 255
-- TODO: add a timer for the simulation updating
-- as this is way too fast and slowing it down affects the GUI
fps = 1/60
cellSize = 16

main :: IO ()
main = getArgs >>= handle

handle :: [String] -> IO ()
handle [n]       = runGlider $ read n
handle _         = runSDLGlider

runGlider :: Int -> IO ()
runGlider n = do
  start <- getPOSIXTime 
  loop glider' start start
  where glider' = glider n
        loop :: Simulation -> POSIXTime -> POSIXTime -> IO ()
        loop g last now = do
          prettyPrint g
          when (now - last < fps) $
            threadDelay $ toMicros (fps - (now - last))
          loop (advance g) now =<< getPOSIXTime


toMicros = round . (* 1000000)

runSDLGlider :: IO ()
runSDLGlider = do
  SDL.initialize [SDL.InitVideo]
  window  <- SDL.createWindow "Life" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenScale screenScale }
  sprite <- SDL.loadBMP "assets/cell.bmp"
  start <- getPOSIXTime
  loop sprite window (glider $ fromIntegral $ screenScale `div` cellSize) start start
  SDL.destroyWindow window
  SDL.quit
  where 
    loop sprite win sim last now = do
      events <- SDL.pollEvents
      surf   <- SDL.getWindowSurface win
      SDL.surfaceFillRect surf Nothing white
      forM_ (elems sim) $ \cell -> do 
        t <- translate sprite cell
        unless (not $ state cell) $ do
          SDL.surfaceBlit sprite Nothing surf (Just t) 
          return ()
      SDL.updateWindowSurface win
      when (now - last < fps) $
        threadDelay $ toMicros (fps - (now - last))
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events in
        if quit then return () else loop sprite win (advance sim) now =<< getPOSIXTime

    translate :: MonadIO m => SDL.Surface -> Cell -> m (SDL.Point SDL.V2 CInt)
    translate sprite cell = do
      dims <- SDL.surfaceDimensions sprite
      return $ SDL.P $ dims * (SDL.V2 (fromIntegral y) (fromIntegral x)) 
      where (y, x) = coordinates cell 
