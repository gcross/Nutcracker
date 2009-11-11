-- @+leo-ver=4-thin
-- @+node:gcross.20091110205054.1969:@thin test.hs
-- @@language Haskell

{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "hello_" hello :: IO ()

main = hello
-- @-node:gcross.20091110205054.1969:@thin test.hs
-- @-leo
