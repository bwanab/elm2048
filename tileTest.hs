-- test cases --

import Tiles

r = [[Empty, T2   , Empty, T2],
     [T4   , Empty, T2   , T2],
     [T2   , Empty, Empty, T2],
     [Empty, T2   , T4   , T4]]

-- r when rotated left should be:
rl  = [[Empty, T2   , T4   , Empty],
       [T2   , Empty, Empty, T2   ],
       [T4   , Empty, T2   , Empty],
       [T4   , T2   , T2   , T2   ]]

testrl :: String
testrl = if rl == (rotate TLeft r) then "testrl OK" else "testrl failed"

-- r when rotated right should be:
rr  = [[T2   , T2   , T2   , T4   ],
       [Empty, T2   , Empty, T4   ],
       [T2   , Empty, Empty, T2   ],
       [Empty, T4   , T2   , Empty]]

testrr :: String
testrr = if rr == (rotate TRight r) then "testrr OK" else "testrr failed"

-- r when swiped top to bottom should be:
ttb = [[Empty, Empty, Empty, Empty],
       [Empty, Empty, Empty, Empty],
       [T4   , Empty, T2   , T2   ],
       [T2   , T4   , T4   , T8   ]]

testttb :: String
testttb = if ttb == (swipe TopToBottom r) then "testttb OK" else "testttb failed"

-- r when swiped bottom to top should be:
btt = [[T4   , T4   , T2   , T4   ],
       [T2   , Empty, T4   , T2   ],
       [Empty, Empty, Empty, T4   ],
       [Empty, Empty, Empty, Empty]]

testbtt :: String
testbtt = if btt == (swipe BottomToTop r) then "testbtt OK" else "testbtt failed"

testlr :: String
testlr = if rotate TLeft (rotate TRight r) == r then "testlr OK" else "testlr failed"

main = do
     putStrLn testrl
     putStrLn testrr
     putStrLn testttb
     putStrLn testbtt
     putStrLn testlr
