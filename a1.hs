-- CPSC 449: Programming Paradigms
-- Professor: Dr. Ben Stephenson
-- University of Calgary
--
-- Assignment 1
-- Evan Loughlin
-- 00503393
-- June 2, 2017

-- Description:
-- This assignment explores concepts of recursion and randomness in the Haskell programming language,
-- via the implementation of a "Mondrian" - stylized art piece. The artwork is implemented
-- using functions which recursively output lines of varying length until subdivisions of space
-- are limited to small rectangles, at which point they're filled with some colour. The visual
-- aspect is handled by outputted HTML tags.
--
-- I chose to extend this assignment slightly, by simply overlaying silhouettes of various geographic
-- maps. Namely, 1) Canada 2) USA 3) Germany 4) Australia 6) The World
-- To do this, I added new main methods in which the string to HTML file contains inline CSS.
--
-- The theme behind this project was to try to allow users to consider the different ways in which
-- humans sometimes "randomly" group -- and divide -- ourselves, into different categories.
--
-- Algorithmic extensions: 
--     1) Variable line widths
--     2) Colours were randomly selected across the entire selection. 
--     3) A list of random numbers, applied recursively, ensured that several colours were repeated,
--        often in nearby areas. I tried to similate geographical randomness of the world map. 

-- Possible Additions:
--     1) Colour each country, according to their flag's national colours.
--     2) Increase the number of divisions that occur.

import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- 
-- The width and the height of the canvas
--

width :: Int
width = 1024

height :: Int
height = 768

--
-- Max and minimum thicknesses of black lines.
--

maxThk :: Int
maxThk = 5

minThk :: Int
minThk = 2

--
-- Random Number Function 
-- Generate and return a list of 20,000 random floating point numbers between 0 and 1
-- This function is used to add randomness to the output.
--
randomList :: IO [Float]
randomList = replicateM 20000000 $ randomRIO (0.0, 1.0 :: Float)

--
-- Random Integer Function
-- Used to compute an integer from three inputs: an integer lower bound, and integer upper bound,
-- and a Float (between 0 and 1), returning some random value between the lower and the upper integers.
--
randomInt :: Int -> Int -> Float -> Int
randomInt lower upper p = round (fromIntegral(upper - lower) * p) + fromIntegral(lower)

--
-- Black Line Generator
-- Prints out the HTML tag to create a black line, given the following parameters: x coordinate, y coordinate, 
-- width, and height.
--
genBlackLine :: Int -> Int -> Int -> Int -> String
genBlackLine x y w h = 
   "<rect x=" ++ (show x) ++
   " y=" ++ (show y) ++
   " width=" ++ (show w) ++
   " height=" ++ (show h) ++
   " fill= \"black\"/> \n"

-- 
-- Randomly Coloured Rectangle Generator
-- Function, which accepts input of X-Coordinate, Y-Coordinate, Width, Height, and a list of floats 
-- between 0 and 1, which represent (red, green, blue), and outputs an HTML tag string, outputting a square.
--
genColouredRect :: Int -> Int -> Int -> Int-> [Float] -> String
genColouredRect x y w h (r:g:b:rs) = 
   "<rect x=" ++ (show x) ++
   " y=" ++ (show y) ++
   " width=" ++ (show w) ++
   " height=" ++ (show h) ++ 
   " style=\"fill:rgb(" ++ (show (round (r * 255))) ++ "," ++ 
                            (show (round (g * 255))) ++ "," ++
                            (show (round (b * 255))) ++ ")\" />\n"
-- Fill with white, if numbers have run out.
genColouredRect x y w h _ = 
   "<rect x=" ++ (show x) ++
   " y=" ++ (show y) ++
   " width=" ++ (show w) ++
   " height=" ++ (show h) ++ 
   " style=\"fill:rgb(" ++ (show (round (255))) ++ "," ++ 
                            (show (round (255))) ++ "," ++
                            (show (round (255))) ++ ")\" />\n"
-- 
-- Generate Mondrian Art 
-- A standard generation of mondrian art for this assignment.
-- A function which recursively calls itself
-- Input into function: X-coordinate, Y-Coordinate, Width, Height (of canvas)
--

genMondrian :: Int -> Int -> Int -> Int -> [Float] -> String
genMondrian x y w h [] = genColouredRect x y w h [0.0]
genMondrian x y w h (r:r2:rs)
-- 1) Split into 4 Large regions:
   | w > width `div` 2 && h > height `div` 2 = 
      genMondrian x y (randomInt x w r) h (tail rs) ++ 
      genMondrian (x+(randomInt x w r)) y (w-(randomInt x w r)) h (tail rs) ++ 
      genBlackLine x y (randomInt minThk maxThk r) h
-- 2) Subdivide regions by height:
   | h > 500 = 
     genMondrian x y w (randomInt y h r) (tail rs) ++
     genMondrian x (y+(randomInt y h r)) w (h-(randomInt y h r)) (tail rs) ++
     genBlackLine x y w (randomInt minThk maxThk r)
-- 3) Subdivide regions by width:
   | w > 500 =
     genMondrian x y (randomInt x w r) h (tail rs) ++
     genMondrian (x+randomInt x w r) y (w-(randomInt w x r)) h (tail rs) ++
     genBlackLine x y (randomInt minThk maxThk r) h
-- 4) Once subdivided, colour the remaining section.
   | otherwise = genColouredRect x y w h (tail rs) ++
     genBlackLine x y (randomInt minThk maxThk r) h ++ genBlackLine x y w (randomInt minThk maxThk r)
-- 
-- The main program, which generates and outputs a1.html
--
main :: IO ()
main = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height=\"" ++ (show height) ++ "\">\n"
       image = genMondrian 0 0 width height randomValues ++ 
               -- Create Frame for Canvas
               (genBlackLine 0 0 10 height) ++
               (genBlackLine 0 0 width 10) ++ 
               (genBlackLine (width-10) 0 10 height) ++ 
               (genBlackLine 0 (height-10) width 10)
       suffix = "</svg>\n</html>"
   
   writeFile "a1.html" (prefix ++ image ++ suffix)
   
   
--
-- The second main function (alternate). Canada version.
--
main2 :: IO ()
main2 = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height=\"" ++ (show height) ++ "\">\n" ++
                "<style> \n z-index:10; \n div {\n position: absolute; \n } \n" ++
                "svg { \n position:absolute; \n left: 8; \n top: 8; \n z-index: -1; \n }\n </style>"
       image = genMondrian 0 0 width height randomValues
       suffix = "</svg>\n<img src=\"https://i.imgur.com/FHwCc3W.png\" width=\"" ++ show width ++ "\" height = \"" ++ show height ++ "\"></html>"
   
   writeFile "a1_canada.html" (prefix ++ image ++ suffix)
--
-- The third main function (alternate). USA version.
--
main3 :: IO ()
main3 = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height=\"" ++ (show height) ++ "\">\n" ++
                "<style> \n z-index:10; \n div {\n position: absolute; \n } \n" ++
                "svg { \n position:absolute; \n left: 8; \n top: 8; \n z-index: -1; \n }\n </style>"
       image = genMondrian 0 0 width height randomValues
       suffix = "</svg>\n<img src=\"https://i.imgur.com/di69jBf.png\" width=\"" ++ show width ++ "\" height = \"" ++ show height ++ "\"></html>"
   
   writeFile "a1_usa.html" (prefix ++ image ++ suffix)
--
-- The fourth main function (alternate). Deutschland version.
--
main4 :: IO ()
main4 = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height=\"" ++ (show height) ++ "\">\n" ++
                "<style> \n z-index:10; \n div {\n position: absolute; \n } \n" ++
                "svg { \n position:absolute; \n left: 8; \n top: 8; \n z-index: -1; \n }\n </style>"
       image = genMondrian 0 0 width height randomValues
       suffix = "</svg>\n<img src=\"https://i.imgur.com/N74laxN.png\" width=\"" ++ show width ++ "\" height = \"" ++ show height ++ "\"></html>"
   
   writeFile "a1_deutschland.html" (prefix ++ image ++ suffix)
--
-- The fifth main function (alternate). Australia version.
--
main5 :: IO ()
main5 = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg width=\"" ++ (show width) ++
                "\" height=\"" ++ (show height) ++ "\">\n" ++
                "<style> \n z-index:10; \n div {\n position: absolute; \n } \n" ++
                "svg { \n position:absolute; \n left: 8; \n top: 8; \n z-index: -1; \n }\n </style>"
       image = genMondrian 0 0 width height randomValues
       suffix = "</svg>\n<img src=\"https://i.imgur.com/RYYse51.png\" width=\"" ++ show width ++ "\" height = \"" ++ show height ++ "\"></html>"
   
   writeFile "a1_australia.html" (prefix ++ image ++ suffix)
--
-- The sixth main function (alternate). World Map version.
--
main6 :: IO ()
main6 = do
   randomValues <- randomList 

   let prefix = "<html><head></head><body>\n" ++ 
                "<svg height=\"" ++ (show height) ++
                "\" width=\"1360\">\n" ++
                "<style> \n z-index:10; \n div {\n position: absolute; \n } \n" ++
                "svg { \n position:absolute; \n left: 8; \n top: 8; \n z-index: -1; \n }\n </style>"
       image = genMondrian 0 0 1360 height randomValues
       suffix = "</svg>\n<img src=\"https://i.imgur.com/96lsQZN.png\" width=\"1360\" height = \"" ++ show height ++ "\"></html>"
   
   writeFile "a1_world.html" (prefix ++ image ++ suffix)
   