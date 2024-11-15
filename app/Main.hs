module Main where

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Graphics.Gloss

import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V

import qualified Data.Vector.Mutable as VM

import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt
import qualified Graphics.Gloss.Data.Color(white)
import Graphics.Gloss.Interface.IO.Simulate
    ( greyN,
      polygon,
      simulateIO,
      makeColor,
      Display(InWindow),
      Color,
      Picture(ThickCircle, Pictures, Color, Translate),
      Point )
import System.Random


width :: Float
width = 32

type RGBAofColor = (Float,Float,Float,Float)

data Maze = Maze
  { --whiteBall :: Ball,
    -- baseCells :: [(RGBAofColor, (Float, Float))],
    baseCellsUV :: V.Vector (RGBAofColor, (Float, Float))
  }

main :: IO ()
main = do
  directions <- getStdGen <&> take 4 . randomRs (0, 360)
  simulateIO
    (InWindow "Maze - algorithms" (1800, 1600) (100, 100))
    white 
    100 -- number of steps per second, bigger => faster
    ( startGame
        directions
    )
    renderToPicture
    stepWorld

startGame :: [Float] -> Maze
startGame dirs =
  let 
      cells = [(x - width/2 ,y-width/2) | x<- [0..width], y <- [0..width]]
   in Maze {
            baseCellsUV = V.fromList $ (,) `zipWith` repeat (rgbaOfColor black) $ cells}

stepWorld :: p -> Float -> Maze -> IO Maze
stepWorld _ t y@(Maze baseCellsUV) = 
  do
   mycolorRaw <- randomRGBRaw
   [i] :: [Int] <-  newStdGen <&> take 1 . randomRs (0, round width ^2)
   let (colUV, coordsUV) = baseCellsUV ! i
  --  let newCells = take i baseCells <> [(mycolorRaw, coords)] <> drop (i + 1) baseCells
   let newCellsUV = baseCellsUV // [(i, (mycolorRaw, coordsUV))]
   return $ Maze newCellsUV

renderToPicture :: Maze -> IO Picture
-- renderToPicture (Maze whiteBall@(Ball p v) baseCellsUV) =
renderToPicture (Maze baseCellsUV) = do

  -- [i] :: [Int] <-  newStdGen <&> take 1 . randomRs (0, 3)
  return $ Color black $
    Pictures $
        
        let s = 1000/width in
        map
          ( \(c@(r,g,bl,z),(a, b)) -> if c == rgbaOfColor black then 

              Color black $ Translate (a*s) (b*s) (line [(0,0), (0,s), (s, s), (s,0), (0,0)] )
              else
              Color (makeColor r g bl z) $ Translate (a*s) (b*s)  (polygon [(0,0), (0,s), (s, s), (s,0), (0,0)] )
          )
          (V.toList baseCellsUV)

randomRGB :: IO Color
randomRGB = do
  [r,b,g] :: [Float] <- newStdGen <&> take 3 . randomRs (0, 0.99999)
  return $ makeColor r b g 1.0

randomRGBRaw :: IO RGBAofColor
randomRGBRaw = rgbaOfColor <$> randomRGB
