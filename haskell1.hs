convertToHex :: Int -> String
convertToHex 10 = "A"
convertToHex 11 = "B"
convertToHex 12 = "C"
convertToHex 13 = "D"
convertToHex 14 = "E"
convertToHex 15 = "F"
convertToHex m = convertToChar m

convertToChar :: Int -> String
convertToChar 0 = "0"
convertToChar 1 = "1"
convertToChar 2 = "2"
convertToChar 3 = "3"
convertToChar 4 = "4"
convertToChar 5 = "5"
convertToChar 6 = "6"
convertToChar 7 = "7"
convertToChar 8 = "8"
convertToChar 9 = "9"

convertDigits :: Int -> String
convertDigits 0 = "0"
convertDigits m
  | m > 9 && m < 16 = convertDigits (m - 1) ++ convertToHex m
  | otherwise = convertDigits (m - 1) ++ convertToChar m


permutation :: String -> Int -> Int -> Int -> String
permutation tbl 0 _ _ = tbl
permutation tbl n m c = 
    let
        tmp_m = m
        tmp_n = n - 1
        temp_x = drop (tmp_m) tbl
        temp_p = take (tmp_m-1+c) temp_x
        temp_z = tbl !! (tmp_m-1)
        
        tbl' = take (tmp_m-1) tbl ++ temp_p ++ [temp_z] ++ drop (m+1+c) tbl
    in
        permutation tbl' (n-1) (m-1) (c+1)


triangularNumber :: [Int] -> String
triangularNumber [n, m] =
  let
    c = 1
    numbers = convertDigits m
    numbers1 = permutation numbers (n-1) m c
    
  in
    numbers1

main :: IO ()
main = do
  print(triangularNumber [1, 3])
  print(triangularNumber [2, 3])
  print(triangularNumber [3, 3])
  print(triangularNumber [4, 3])
  print(triangularNumber [1, 10])
  print(triangularNumber [2, 10])
  print(triangularNumber [3, 10])
  print(triangularNumber [4, 10])
  print(triangularNumber [5, 10])
  print(triangularNumber [6, 10])
  print(triangularNumber [7, 10])
  print(triangularNumber [8, 10])
  print(triangularNumber [9, 10])
  print(triangularNumber [10, 10])
  print(triangularNumber [11, 10])


