{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Data.Char (ord, chr, toUpper)
import Data.Ix (inRange)
import Test.QuickCheck
import Control.Applicative

foreign import ccall "ceasar.h caesar"
    c_caesar :: CInt -> CString -> CString

native_caesar :: Int -> String -> IO String
native_caesar shift input = withCString input $ \c_str -> peekCString(c_caesar (fromIntegral shift) c_str)

caesar :: Int -> String -> String
caesar k = map f
  where
    f c
        | inRange ('a','z') c || inRange ('A','Z') c = chr $ ord 'A' + (ord (toUpper c) - ord 'A' + k) `mod` 26
        | otherwise = c

unCaesar :: Int -> String -> String
unCaesar k = caesar (-k)

unsafeEq :: IO String -> String -> Bool
unsafeEq x y = unsafePerformIO(x) == y

genSafeChar :: Gen Char
genSafeChar = elements ['A'..'Z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
  arbitrary = SafeString <$> genSafeString

equivalenceProperty = forAll genSafeString $ \str -> unsafeEq (native_caesar 2 str) (caesar 2 str)

deepCheck p = quickCheckWith stdArgs{ maxSuccess = 1000000 } p
