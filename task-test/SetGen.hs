{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module SetGen where

import Control.Monad
import Data.List (intercalate)
import Test.QuickCheck

import Text.Task.Parser.Types (RawEntry, todoTagStr)

data RawTag     = RawTag RawEntry    deriving Show
data Number     = Number String      deriving Show
data RNumber    = RNumber String     deriving Show
data LabelList  = LabelList [String] deriving Show
data AttrSec    = AttrSec String     deriving Show
data Priority   = Priority String    deriving Show
data SourcePath = SourcePath String  deriving Show
data Date       = Date String        deriving Show

class Variant a where
  valid   :: Gen a
  invalid :: Gen a

instance Variant a => Arbitrary a where
  arbitrary   = oneof [valid {-, invalid-}]

instance Variant Number where
  valid   = liftM Number $ resize 4 . listOf' $ elements "0123456789"
  invalid = liftM Number $ listOf $ elements (['A'..'Z'] ++ ['a' .. 'z'] ++ 
                                               " .~!@#$%^<>&*(),;|:\n\r\t")
instance Variant RNumber where
  valid   = liftM RNumber $ 
          (valid >>= \(Number n1) -> 
             valid >>= \(Number n2) -> 
               if not (null n1) && not (null n2)
                 then return $ n1 ++ "." ++ n2
                 else return $ n1 ++ n2)
  invalid = liftM RNumber $ listOf $ elements 
                                    (['A'..'Z'] ++ ['a' .. 'z'] ++ 
                                     " ~!@#$%^<>&*(),;|:\n\r\t")

instance Variant Priority where
  valid   = liftM Priority $ elements ["", "L", "N", "H"]
  invalid = undefined

instance Variant LabelList where
  valid   = liftM LabelList 
            (do l <- listOf . listOf' $ elements 
                     (['A'..'Z'] ++ ['a' .. 'z'] ++ ['0'..'9'])
                mapM (\s-> do h <- elements ["", "#"]
                              return $ if null s then s else h++s) l)
  invalid = liftM LabelList $ listOf . listOf' $ elements
          " ~!@$%^&*(),;|:\n\r\t"

instance Variant AttrSec where
  valid   = liftM AttrSec $ listOf' $ elements 
          (['A'..'Z'] ++ ['a' .. 'z'] ++ ['0'..'9'] ++ 
           " ~!@#$<>%^&*(),;:")
  invalid = liftM AttrSec $ listOf' $ elements
          "|\n\r\t"

instance Variant SourcePath where
  valid   = liftM SourcePath $ listOf' $ elements 
          (['A'..'Z'] ++ ['a' .. 'z'] ++ ['0'..'9'] ++ 
           " /\\_(),.$#@'")
  invalid = liftM SourcePath $ listOf' $ elements
          "|\n\r\t\""

instance Variant Date where
  valid   = liftM Date
        (do d  <- elements ["", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
            m  <- elements ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                            "Aug", "Sep", "Oct", "Nov", "Dec"]
            dm <- elements [1..31]
            hh <- elements [0..23]
            mm <- elements [0..59]
            ss <- elements [0..59]
            z  <- elements ["SAST", "PST", "EST", "UTC"]
            y  <- elements [1990..2010]
            if null d -- No arb day of week? Provide empty str for Date
              then return ""
              else return $ d ++ " " ++ m ++ " " ++ show dm ++ " " ++ 
                            tStr hh ++ ":" ++ tStr mm ++ ":" ++ tStr ss ++ 
                            " " ++ z ++ " " ++ show y)
    where tStr n = if n < 10 then "0" ++ show n else show n
  invalid = undefined

instance Variant RawTag where
  valid = liftM RawTag 
        (do valid >>= \(LabelList ul) -> 
              valid >>= \(AttrSec act) ->
                valid >>= \(AttrSec mil) ->
                  valid >>= \(Priority pr) ->
                    valid >>= \(RNumber ts) ->
                      valid >>= \(Number lNo) ->
                        valid >>= \(Number cNo) ->
                          valid >>= \(SourcePath sp) ->
                            valid >>= \(Date dm) -> 
                              return $ todoTagStr ++ (uStr ul) ++ ": " ++ 
                                       mStr mil ++ act ++ 
                                       pStr pr ++ tStr ts pr ++ 
                                       sStr sp ++ ", (" ++ 
                                       reqNum lNo ++ "," ++ 
                                       reqNum cNo ++ ")" ++ dStr dm)
    where uStr ul  = if null ul then "" 
                     else "(" ++ (intercalate ", " ul) ++ ")"
          mStr m   = if null m then "" else m ++ atSep
          pStr p   = if null p then "" else atSep ++ p
          tStr t p = if null t then "" else 
                        case not $ null p of
                          True      -> ", " ++ t
                          otherwise -> atSep ++ t
          reqNum n = if null n then "0" else n
          sStr s   = atSep ++ "\"" ++ 
                     (if null s then "unknown" else s) ++ "\""
          dStr d   = if null d then "" else ", " ++ d
          atSep    = " | "
  invalid = undefined --liftM RawTag ???

listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf' k gen
 
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]
