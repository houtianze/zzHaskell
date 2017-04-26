removeHT :: String -> String
removeHT []  = []
removeHT [a] = [a]
removeHT xs  = drop 1 $ take (length xs - 1)  xs

rpn :: (Num a, Read a) => String -> a
rpn = head . foldl fn [] . words
  where fn (x:y:ys) "*" = (x * y):ys
        fn xs ns        = read ns:xs

repl = do
  exp <- getLine
  print $ show $ rpn $ exp
  repl

main = repl
