import Indexable

main :: IO ()

main = do
  let lst = [0..10]       -- create a list
  putStrLn $ show $ lst&[""]            -- select all elements
  putStrLn $ show $ lst&["0,3,8"]       -- select elements of index 0, 3 and 8
  putStrLn $ show $ lst&["2:"]          -- select all elements from index 2 to the end
  putStrLn $ show $ lst&[":-1"]         -- select all elements except the last one
  putStrLn $ show $ lst&["1:-1:2"]      -- select elements in range with step size 2
  putStrLn $ show $ lst&["1:3,4:6,8"]   -- elements 1:3, 4:6 and the 8th.
