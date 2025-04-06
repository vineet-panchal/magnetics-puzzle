module Main where
    import Test.HUnit
    import Data.List
    import Data.Array.IO
    import System.Random
    import Control.Monad
    import Data.Char (isSpace)
    import Data.Time.Clock.POSIX (getPOSIXTime)
    import Data.Time
    import System.Timeout (timeout)
    import qualified Polarity
    import qualified Data.Set as Set
    import qualified System.Exit as Exit

    main = do
        status <- runTestTT all_tests
        if failures status > 0 || errors status > 0 then Exit.exitFailure else Exit.exitSuccess

    polarity = Polarity.polarity
       
    all_tests = TestList $ test_1_5x6 ++ 
                           test_2a_2x2 ++ test_2b_2x2 ++ 
                           test_3a_4x4 ++ test_3b_4x4 ++ 
                           test_4a_8x8 ++ test_4b_8x8 
                          --  test_5_16x16

    test_1_5x6 = 
        [ TestCase $ do
        start <- getCurrentTime
    
        let specs = ( [2,3,-1,-1,-1], [-1,-1,-1,1,-1], [1,-1,-1,2,1,-1], [2,-1,-1,2,-1,3] )
        let board = [ "LRLRTT", "LRLRBB", "TTTTLR", "BBBBTT", "LRLRBB" ]

        writeFile "hs_log.txt" $ "1  -   5x6: "

        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "1: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "1: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "1: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "1: Constraints not satisfied!" a4 True
    
        end <- getCurrentTime
        let t1 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t1) ++ " ms"

        assertEqual "" True True ]

    test_2a_2x2 = 
        [ TestCase $ do
        start <- getCurrentTime
    
        let specs = ( [1, 1], [1, 1], [1, 1], [1, 1] )
        let board = [ "TT", "BB" ]

        appendFile "hs_log.txt" $ "\n2a -   2x2: "

        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "2a: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "2a: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "2a: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "2a: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"
       
        assertEqual "" True True ]

    test_2b_2x2 = 
        [ TestCase $ do
        start <- getCurrentTime
   
        let specs = ( [1, -1], [1, -1], [1, -1], [-1, 1] )
        let board = [ "LR", "LR" ]
  
        appendFile "hs_log.txt" $ "\n2b -   2x2: "
 
        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "2b: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "2b: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "2b: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "2b: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"
        
        assertEqual "" True True ]

    test_3a_4x4 = 
        [ TestCase $ do
        start <- getCurrentTime
  
        let specs = ( [0, 1, 2, -1], [0, -1, 1, 2], [1, 1, -1, 1], [1, 1, 0, 2] )
        let board = [ "TTLR", "BBLR", "LRTT", "LRBB" ]
        
        appendFile "hs_log.txt" $ "\n3a -   4x4: "
 
        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "3a: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "3a: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "3a: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "3a: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"

        assertEqual "" True True ]

    test_3b_4x4 = 
        [ TestCase $ do
        start <- getCurrentTime
   
        let specs = ( [1, 2, -1, -1], [-1, 2, -1, 2], [2, -1, 0, 2], [2, -1, 2, 2] )
        let board = [ "TLRT", "BLRB", "TLRT", "BLRB" ]
        
        appendFile "hs_log.txt" $ "\n3b -   4x4: "

        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "3b: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "3b: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "3b: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "3b: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"

        assertEqual "" True True ]

    test_4a_8x8 = 
        [ TestCase $ do
        start <- getCurrentTime
    
        let specs = ( [-1, -1, 2, 2, 4, -1, 3, 2], [-1, 1, -1, 3, 3, -1, -1, 4],
                        [0, 4, 3, 3, -1, 3, -1, 1], [2, 2, 3, 3, 2, -1, 1, 3] )
        let board = [ "LRTTLRTT", "LRBBLRBB", "TTLRTTLR", "BBLRBBLR", 
                      "LRTTLRTT", "LRBBLRBB", "TTLRTTLR", "BBLRBBLR"]
        
        appendFile "hs_log.txt" $ "\n4a -   8x8: "
   
        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "4a: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "4a: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "4a: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "4a: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"

        assertEqual "" True True ]

    test_4b_8x8 = 
        [ TestCase $ do
        start <- getCurrentTime
    
        let specs = ( [-1, 2, 2, 2, 2, 2, 2, 0], [1, 1, 1, -1, 3, 3, -1, -1],
                        [1, 0, 2, 3, 2, -1, 2, 2], [0, 2, 1, 3, 2, 2, 1, -1] )
        let board = [ "LRLRLRLR", "LRLRTLRT", "TTTTBTTB", "BBBBTBBT", 
                      "LRLRBTTB", "TLRTTBBT", "BLRBBLRB", "LRLRLRLR"]
        
        appendFile "hs_log.txt" $ "\n4b -   8x8: "
 
        let sol = polarity board specs
        let a1 = validateDimensions sol board
        assertEqual "4b: Solution and board dimensions don't match!" a1 True
        let a2 = validatePlacement sol board
        assertEqual "4b: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
        let a3 = validatePoles sol
        assertEqual "4b: Found same adjecent polarity! (++ or --)" a3 True
        let a4 = validateConstraints sol specs
        assertEqual "4b: Constraints not satisfied!" a4 True
        
        end <- getCurrentTime
        let t2 = round $ (diffUTCTime end start) * 1000
        appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"

        assertEqual "" True True ]

    -- test_5_16x16 = 
    --     [ TestCase $ do
    --     start <- getCurrentTime
    
    --     let specs = ( [-1, -1, -1, 2, 1, 4, 1, 2, 0, 2, -1, 3, 2, -1, -1, 1], 
    --                     [0, 0, 3, 2, 1, 2, 3, 2, 0, 1, -1, -1, 2, 1, 1, 1],
    --                     [1, 0, 0, 1, -1, 3, 2, 2, 1, -1, 3, 1, 2, -1, 2, -1],
    --                     [1, -1, -1, 1, 3, 1, 3, 1, 0, 2, -1, 3, 2, 3, 2, 0] )
    --     let board = [ "LRLRTTTTTTTTLRLR", "LRLRBBBBBBBBLRLR", "LRTTTTLRTTLRLRTT", "LRBBBBLRBBLRLRBB",  
    --                   "LRLRLRLRLRLRLRLR", "TLRTTLRTTLRTTLRT", "BLRBBLRBBLRBBLRB", "TTLRLRLRTTLRTTLR", 
    --                   "BBLRLRLRBBLRBBLR", "TTLRTLRTLRTTTTLR", "BBLRBLRBLRBBBBLR", "LRLRLRLRLRLRLRLR",
    --                   "LRLRTTLRLRLRTTTT", "LRLRBBLRLRLRBBBB", "TLRTTTTTTLRTTTTT", "BLRBBBBBBLRBBBBB" ]
        
    --     appendFile "hs_log.txt" $ "\n5  - 16x16: "
   
    --     let sol = polarity board specs
    --     let a1 = validateDimensions sol board
    --     assertEqual "5: Solution and board dimensions don't match!" a1 True
    --     let a2 = validatePlacement sol board
    --     assertEqual "5: Found illegal tile placement! Must be one of XX, +-, or -+" a2 True
    --     let a3 = validatePoles sol
    --     assertEqual "5: Found same adjecent polarity! (++ or --)" a3 True
    --     let a4 = validateConstraints sol specs
    --     assertEqual "5: Constraints not satisfied!" a4 True
        
    --     end <- getCurrentTime
    --     let t2 = round $ (diffUTCTime end start) * 1000
    --     appendFile "hs_log.txt" $ "PASSED in " ++ (show t2) ++ " ms"

    --     assertEqual "" True True ]

    validateDimensions :: [String] -> [String] -> Bool
    validateDimensions sol board =
        let sameHeight = length sol == length board
            sameWidth  = all (\(x, y) -> length x == length y) (zip sol board)
        in sameHeight && sameWidth

    validatePlacement :: [String] -> [String] -> Bool
    validatePlacement sol board =
        let rows = length board
            cols = length (head board)
            tiles = [ getTiles sol board r c | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ]
        in all (\(s, _) -> s == ('X', 'X') || s == ('+', '-') || s == ('-', '+')) tiles

    transp :: [[a]] -> [[a]]
    transp []          = []
    transp ([] : _)    = []
    transp m           = map head m : transp (map tail m)

    getTiles :: [String] -> [String] -> Int -> Int -> ((Char, Char), (Char, Char))
    getTiles sol board x y =
        let b1 = (board !! x) !! y 
            s1 = (sol !! x) !! y    
            (b2, s2) = case b1 of
                'T' -> ((board !! (x + 1)) !! y, (sol !! (x + 1)) !! y)
                'B' -> ((board !! (x - 1)) !! y, (sol !! (x - 1)) !! y)
                'L' -> ((board !! x) !! (y + 1), (sol !! x) !! (y + 1))
                'R' -> ((board !! x) !! (y - 1), (sol !! x) !! (y - 1))
                _   -> error "Invalid direction"  
        in ((s1, s2), (b1, b2))

    validatePoles :: [String] -> Bool
    validatePoles sol =
        let rows = length sol
            cols = length (head sol)
            rowPoles = [ ((sol !! r) !! c, (sol !! r) !! (c + 1)) | r <- [0 .. rows - 1], c <- [0 .. cols - 2] ]
            rowPsd = all (\(a, b) -> a == 'X' || a /= b) rowPoles
            colPoles = [ ((sol !! r) !! c, (sol !! (r + 1)) !! c) | c <- [0 .. cols - 1], r <- [0 .. rows - 2] ]
            colPsd = all (\(a, b) -> a == 'X' || a /= b) colPoles
        in colPsd && rowPsd   

    validateConstraints :: [String] -> ([Int], [Int], [Int], [Int]) -> Bool
    validateConstraints sol (leftCons, rightCons, topCons, bottomCons) =
        let sliRows = map (map (:[])) sol  
            sliCols = transp sliRows
            plusRow = map (length . filter (== "+")) sliRows
            plusCol = map (length . filter (== "+")) sliCols
            minusRow = map (length . filter (== "-")) sliRows
            minusCol = map (length . filter (== "-")) sliCols
            plusRowZip = zip plusRow leftCons
            plusColZip = zip plusCol topCons
            minusRowZip = zip minusRow rightCons
            minusColZip = zip minusCol bottomCons
            plusRowPsd = all (\(s, c) -> c == -1 || s == c) plusRowZip
            plusColPsd = all (\(s, c) -> c == -1 || s == c) plusColZip
            minusRowPsd = all (\(s, c) -> c == -1 || s == c) minusRowZip
            minusColPsd = all (\(s, c) -> c == -1 || s == c) minusColZip
        in plusRowPsd && plusColPsd && minusRowPsd && minusColPsd