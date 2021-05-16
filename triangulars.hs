-- | A triangular number or triangle number counts objects arranged in an equilateral triangle. 
-- Triangular numbers are a type of figurate number, other examples being square numbers and cube numbers. 
-- generateTriangles 15
-- will take the first 15 triangular numbers and display the triangles on the screen.
-- generateOneTriangle n
-- Generate the nth triangular number and graphically display it on the screen
import Data.List

-- | Generate the nth triangular number
triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

-- | Infinitly generate triangular numbers
infTriangulars :: [Int]
infTriangulars = [triangular x | x <- [1..]]

-- | Build a triangular list of '*'
buildTriangle :: Int -> Int -> [[Char]]
buildTriangle 0 _ = []
buildTriangle max x = (intersperse ' ' $ concat $ replicate x "*") : (buildTriangle (max-x) (x+1))

-- | Show a triangular list of '*' with right padding
showTriangle :: [[Char]] -> Int -> IO ()
showTriangle xs ptr 
        | ptr == length xs = do return ()
        | otherwise = do 
                      let s = (length xs)-ptr-1
                      putStr (concat $ replicate s " ")
                      putStrLn (xs !! ptr)
                      showTriangle xs (ptr+1)

-- | Display a triangular number
displayTriangle :: Int -> IO ()
displayTriangle x = do putStrLn ("Number :" ++ show x)
                       showTriangle trg 0
                       where trg = buildTriangle x 1

-- | Show a number of triangular numbers
showTriangles :: [Int] -> IO ()
showTriangles []     = return ()
showTriangles (x:xs) = do displayTriangle x
                          showTriangles xs

-- | Generate a number of triangular numbers and graphically display them on the screen
generateTriangles :: Int -> IO ()
generateTriangles x = do showTriangles (take x infTriangulars)

-- | Generate the nth triangular number and graphically display it on the screen
generateOneTriangle :: Int -> IO ()
generateOneTriangle x = do displayTriangle (infTriangulars !! x)
