module Data.CPF (Cpf(..)
                , from
                , valid
                , format
                , unformat
                ) where

import           Data.Char     (digitToInt, isDigit)
import Data.Monoid (Any(..))

data Cpf = Invalid | Valid String deriving (Eq, Show)

from :: String -> Cpf
from s = if valid' unformatted
               then Valid unformatted
               else Invalid
         where unformatted = unformat' s

valid :: Cpf -> Bool
valid Invalid   = False
valid (Valid _) = True

format :: Cpf -> Maybe String
format Invalid = Nothing
format (Valid s) = Just $ take 3 s ++ "." ++ take 3 (drop 3 s) ++ "." ++ take 3 (drop 6 s) ++ "-" ++ drop 9 s

unformat :: Cpf -> Maybe String
unformat Invalid   = Nothing
unformat (Valid s) = Just s

unformat' :: String -> String
unformat' = filter (\c -> c /= '.' && c /= '-')

valid' :: String -> Bool
valid' = getAny . foldMap (Any .) predicates
   where predicates = [hasOnlyDigits, hasElevenDigits, isNotSameDigits, digitsCheck]

firstDigits :: Int -> String -> [Int]
firstDigits n s = map digitToInt $ take n s

verificationDigit :: String -> [Int]
verificationDigit s = map digitToInt $ drop 9 s

digitsCheck ::  String -> Bool
digitsCheck cpf = firstDigit == firstDigitCalculated && secondDigit == secondDigitCalculated
  where
    firstDigit = head (verificationDigit cpf)
    firstDigitCalculated = digitsCalculation 9 [10,9..2]
    secondDigit = last (verificationDigit cpf)
    secondDigitCalculated = digitsCalculation 10 [11, 10..2]
    digitsCalculation n xs = if d == 10 then 0 else d
      where d = 10 * sum (zipWith (*) (firstDigits n cpf) xs) `mod` 11

isNotSameDigits :: String -> Bool
isNotSameDigits s = not . and $ zipWith (==) s (drop 1 s)

hasElevenDigits :: String -> Bool
hasElevenDigits s = 11 == length s

hasOnlyDigits :: String -> Bool
hasOnlyDigits = all isDigit
