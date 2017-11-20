--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: IO [Float]
randomList = replicateM 20000 $ randomRIO (0.0, 1.0 :: Float)

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Draw a rectangle with random color.  Replace the implementation of this
-- function so that it draws a piece of random Mondrian art.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs) = 
  (rs, "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"None\"" ++
       " fill=\"rgb(" ++ (show (round (r * 255))) ++ "," ++
                         (show (round (s * 255))) ++ "," ++
                         (show (round (t * 255))) ++ ")\" />\n")

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  randomValues <- randomList

  let prefix = "<html><head></head><body>\n" ++ 
               "<style> \n z-index:10; \n div {\n position: relative; \n } \n" ++
               "svg { \n position:absolute; \n left: 0; \n top: 0; \n z-index: -1; \n }\n </style>"
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n<img src="</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
--
-- The second main function. Canada version.
--
main :: IO ()
main = do
  randomValues <- randomList

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n<img src="https://i.imgur.com/FHwCc3W.png" width=\"" ++ width ++ "\" height = \"" ++ height ++ "\"></html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)