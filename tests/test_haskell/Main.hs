{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Main
Description : Haskell FFI Test for connector-service-FFI
License     : MIT

Run: cd tests/test_haskell && cabal run

Prerequisites:
  1. Build with C-FFI: cargo build --release --features c-ffi
  2. Generate headers: cbindgen --config cbindgen.toml --crate ffi --output bindings/headers/connector_service.h
  3. Ensure libffi.dylib/so is in library path
-}
module Main where

import Foreign.C.String

-- | FFI import for authorize_c function from Rust library
foreign import ccall "authorize_c" 
    authorize_c :: CString -> IO CString

-- | FFI import for free_string function from Rust library  
foreign import ccall "free_string"
    free_string :: CString -> IO ()

-- | Test request JSON
testRequest :: String
testRequest = "{\"amount\":1000,\"minor_amount\":100,\"payment_id\":\"pay_123\",\"confirm\":true,\"merchant_account\":\"merchant_1\",\"payment_method\":{\"card\":{\"card_number\":4111111111111111,\"card_cvc\":123,\"card_issuer\":null}}}"

-- | Call the Rust authorize function
authorize :: String -> IO String
authorize request = do
    requestC <- newCString request
    resultC <- authorize_c requestC
    result <- peekCString resultC
    free_string resultC
    return result

main :: IO ()
main = do
    result <- authorize testRequest
    putStrLn "Response:"
    putStrLn result