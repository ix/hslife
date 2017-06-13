{-# LANGUAGE OverloadedStrings #-}
module Main where

import Logic.Life
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless)
import Data.Array (elems)
import Foreign.C.Types
import Control.Monad.IO.Class (MonadIO)
import qualified SDL

screenSize  = SDL.V2 800 600
white = SDL.V4 255 255 255 255

main :: IO ()
main = getArgs >>= handle

handle :: [String] -> IO ()
handle [n, mode] =
  case mode of
    "sdl"      -> runSDLGlider $ read n
    "terminal" -> runGlider $ read n
    _          -> putStrLn "only 'sdl' and 'terminal' supported"

handle [n]       = runGlider $ read n
handle _         = do
  putStrLn "defaulting to terminal output"
  runGlider 10

runGlider :: Int -> IO ()
runGlider n = loop glider' 
  where glider' = glider n
        loop g = do
          prettyPrint g
          threadDelay 50000
          loop $ advance g


runSDLGlider :: Int -> IO ()
runSDLGlider n = do
  SDL.initialize [SDL.InitVideo]
  window  <- SDL.createWindow "Life" SDL.defaultWindow { SDL.windowInitialSize = screenSize }
  loop window (glider n)
  SDL.destroyWindow window
  SDL.quit
  where 
    loop win sim = do
      events <- SDL.pollEvents
      surf   <- SDL.getWindowSurface win
      sprite <- SDL.loadBMP "assets/cell.bmp"
      SDL.surfaceFillRect surf Nothing white
      forM_ (elems sim) $ \cell -> do 
        t <- translate sprite cell
        unless (not $ state cell) $ do
          SDL.surfaceBlit sprite Nothing surf (Just t) 
          return ()
      SDL.updateWindowSurface win
      threadDelay 31000
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events in
        if quit then return () else loop win $ advance sim

    translate :: MonadIO m => SDL.Surface -> Cell -> m (SDL.Point SDL.V2 CInt)
    translate sprite cell = do
      dims <- SDL.surfaceDimensions sprite
      return $ SDL.P $ dims * (SDL.V2 (fromIntegral y) (fromIntegral x)) 
      where (y, x) = coordinates cell 
