-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort [] = []
splitSort (x:y:ns)
    | x == y = [fst (eqSub (x:y:[]) ns)]  ++ splitSort (snd (eqSub (x:y:[]) ns))
    | x > y = [fst (decrSub (x:y:[]) ns)] ++ splitSort (snd (decrSub (x:y:[]) ns))
    | x < y = [fst (incrSub (x:y:[]) ns)] ++ splitSort (snd (incrSub (x:y:[]) ns))

eqSub :: Ord a => [a] -> [a] -> ([a],[a])
eqSub sublist [] = (sublist, [])
eqSub sublist (n:ns) =
    if n == (sublist !! 0)
        then eqSub (n:sublist) ns
    else (sublist , n:ns)

decrSub :: Ord a => [a] -> [a] -> ([a],[a])
decrSub sublist [] = (sublist, [])
decrSub sublist (n:ns) =
    if n < sublist !! (length sublist - 1)
        then decrSub (sublist ++ [n]) ns
    else (sublist, n:ns)

incrSub :: Ord a => [a] -> [a] -> ([a],[a])
incrSub sublist [] = (sublist, [])
incrSub sublist (n:ns) = 
    if n > sublist !! (length sublist - 1)
        then incrSub (sublist ++ [n]) ns
    else (sublist, n:ns)

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestcommonSubList [] = []
longestCommonSubList xs = findInAll xs (removeDuplicateNums (concat xs))

--Part i and ii from plan
removeDuplicateNums :: Eq a => [a] -> [a]
removeDuplicateNums [] = []
removeDuplicateNums (n:ns) = n : removeDuplicateNums (filter (/= n) ns)

--Checks all the sublists, then returns the longest sublist.
findInAll :: Eq a => [[a]] -> [a] -> [a]
findInAll [] longestSub = longestSub
findInAll (l:ls) enum = findInAll ls (findInSublist l enum)

--Checks a single sublist for each element of the 
findInSublist :: Eq a => [a] -> [a] -> [a]
findInSublist sl [] = []
findInSublist sl (e:en) =
    if (e `elem` sl)
        then [e] ++ findInSublist sl en
    else 
        findInSublist sl en

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress ms = False

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms = Third 

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = 0.0

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = 0.0

-- Exercise 7
data Instruction = Add | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns [] = ns
executeInstructionSequence ns (i:ins) = executeInstructionSequence ( executeInstruction ns i ) ins

executeInstruction :: [Int] -> Instruction -> [Int]
executeInstruction [_] Pop = []
executeInstruction (a:b:ns) i
    | i == Add && length (a:b:ns) > 1 = a+b : ns
    | i == Multiply && length (a:b:ns) > 1 = a*b : ns
    | i == Duplicate = a:a:b:ns
    | i == Pop && length (ns) > 0 = b:ns
    | i == Pop && length(ns) == 0 = [b]
    | otherwise = ns

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 0 = []
optimalSequence 1 = []
optimalSequence n
    | n == 2 ^ floor (logBase 2 (toEnum(n))) = composeOptimised (floor (logBase 2 (toEnum(n))))
    | n `mod` 2 == 0 = constructSequence (n `div` 2) ++ constructSequence 2 
    | n `mod` 2 == 1 = [Duplicate] ++ optimalSequence (n-1) ++ [Multiply]

constructSequence :: Int -> [Instruction]
constructSequence 1 = []
constructSequence n =
    -- Case where n is a result of 2^x, we only need x combinations of [D,M]
    if n == 2 ^ floor (logBase 2 (toEnum(n))) 
        then composeOptimised (floor (logBase 2 (toEnum(n))))
    else
        composeDuplicate (n - 2^ floor (logBase 2 (toEnum(n)))) ++ composeOptimised (floor (logBase 2 (toEnum(n)))) ++ composeMultiply (n - 2^ floor (logBase 2 (toEnum(n))))

composeDuplicate :: Int -> [Instruction]
composeDuplicate 0 = []
composeDuplicate n = [Duplicate] ++ composeDuplicate (n-1)

composeMultiply :: Int -> [Instruction]
composeMultiply 0 = []
composeMultiply n = [Multiply] ++ composeMultiply (n-1)

composeOptimised :: Int -> [Instruction]
composeOptimised 0 = []
composeOptimised n
    | n > 0 = [Duplicate, Multiply] ++ composeOptimised (n-1)

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [] = []
findBusyBeavers ns = findLargestInstruction ns (executeLargest ns) (findCombos ((length ns) -1))

instruct :: [Instruction]
instruct = [Pop, Add, Multiply]

--Finds all possible permutations of combinations of instructions of a defined length.
findCombos :: Int -> [[Instruction]]
findCombos len = removeDuplicates( [1..len] >>= \len -> mapM (const instruct) [1..len] )

--Removes any duplicate lists of instructions from the list of combinations.
removeDuplicates :: [[Instruction]] -> [[Instruction]]
removeDuplicates [] = []
removeDuplicates (i:is) = i : removeDuplicates (filter (/= i) is)

--Returns the value of the highest result from instructions
executeLargest :: [Int] -> Int
executeLargest [] = 0
executeLargest ns = findLargest ns (findCombos ((length ns)-1)) 0

--Finds the largest possible result from any of the combinations that also results in a singleton result
findLargest :: [Int] -> [[Instruction]] -> Int -> Int
findLargest ns [] max = max
findLargest [] is max = 0
findLargest ns (i:is) max =
    if maximum (executeInstructionSequence ns i) > max && length (executeInstructionSequence ns i) == 1
        then findLargest ns is (maximum (executeInstructionSequence ns i))
    else findLargest ns is max

findLargestInstruction :: [Int] -> Int -> [[Instruction]] -> [[Instruction]]
findLargestInstruction _ _ [] = []
findLargestInstruction ns sum (i:is)
    | (head (executeInstructionSequence ns i)) == sum && length (executeInstructionSequence ns i) == 1 = [i] ++ (findLargestInstruction ns sum is)
    | otherwise = (findLargestInstruction ns sum is)

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = bitDecoder ( convertToNums ( extractDigits s )) ""

--Takes the full string and extracts only 0s and 1s.
extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs) = if  x == '0' || x == '1'
    then x : (extractDigits xs)
    else (extractDigits xs)

--Converts the 0s and 1s (in string form) to int list form
convertToNums :: String -> [Int]
convertToNums str = [read [num] :: Int | num <- str]

--Decodes the list of ints to their respective letters and constructs the string output
bitDecoder :: [Int] -> String -> String
bitDecoder [] str = str 
bitDecoder (a:b:xs) str
    | a == 0 && b == 0 = bitDecoder xs (str ++ "a")
    | a == 0 && b == 1 = bitDecoder xs (str ++ "b")
    | a == 1 && b == 0 = bitDecoder xs (str ++ "c")
    | a == 1 && b == 1 = bitDecoder xs (str ++ "d")

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f 0 0

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False