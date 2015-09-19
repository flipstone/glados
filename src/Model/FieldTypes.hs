{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model.FieldTypes where

import Data.Bits
import Data.Int
import Numeric

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

data MembershipPlanType =
    StudentTinker
  | StudentHacker
  | Hacker
  | HackerHousehold
  | HackerProfessional
  | Sponsor
  deriving (Show, Read, Eq, Enum, Bounded)

membershipPlanTypes :: [MembershipPlanType]
membershipPlanTypes = [minBound .. maxBound]

derivePersistField "MembershipPlanType"


data ReferralSource = ReferralSource {
    referralSearchEngine :: Bool
  , referralTwitter :: Bool
  , referralMeetup :: Bool
  , referralWordOfMouth :: Bool
  , referralConference :: Bool
  , referralOther :: Bool
  }

flag :: Bits a => Int -> Bool -> a
flag _ False = zeroBits
flag n True = bit n

encodeReferralSource :: ReferralSource -> Int64
encodeReferralSource rs =
      flag 0 (referralSearchEngine rs)
  .|. flag 1 (referralTwitter rs)
  .|. flag 2 (referralMeetup rs)
  .|. flag 3 (referralWordOfMouth rs)
  .|. flag 4 (referralConference rs)
  .|. flag 5 (referralOther rs)

decodeReferralSource :: Int64 -> ReferralSource
decodeReferralSource bits = ReferralSource {
    referralSearchEngine  = testBit bits 0
  , referralTwitter       = testBit bits 1
  , referralMeetup        = testBit bits 2
  , referralWordOfMouth   = testBit bits 3
  , referralConference    = testBit bits 4
  , referralOther         = testBit bits 5
  }

instance PersistField ReferralSource where
  toPersistValue = PersistInt64 . encodeReferralSource
  fromPersistValue (PersistInt64 n) = Right (decodeReferralSource n)
  fromPersistValue _ = Left "ReferralSource requires an int64 column type"

instance PersistFieldSql ReferralSource where
  sqlType _ = SqlInt64

data Interests = Interests {
    interest3DPrinting :: Bool
  , interestMetalworking :: Bool
  , interestElectronics :: Bool
  , interestWoodworking :: Bool
  , interestCoworking :: Bool
  , interestGaming :: Bool
  , interestOther :: Bool
  }

encodeInterests :: Interests -> Int64
encodeInterests int =
      flag 0 (interest3DPrinting int)
  .|. flag 1 (interestMetalworking int)
  .|. flag 2 (interestElectronics int)
  .|. flag 3 (interestWoodworking int)
  .|. flag 4 (interestCoworking int)
  .|. flag 5 (interestGaming int)
  .|. flag 6 (interestOther int)

decodeInterests :: Int64 -> Interests
decodeInterests bits = Interests {
    interest3DPrinting =    testBit bits 0
  , interestMetalworking =  testBit bits 1
  , interestElectronics =   testBit bits 2
  , interestWoodworking =   testBit bits 3
  , interestCoworking =     testBit bits 4
  , interestGaming =        testBit bits 5
  , interestOther =         testBit bits 6
  }

instance PersistField Interests where
  toPersistValue = PersistInt64 . encodeInterests
  fromPersistValue (PersistInt64 n) = Right (decodeInterests n)
  fromPersistValue _ = Left "Interests requires an int64 column type"

instance PersistFieldSql Interests where
  sqlType _ = SqlInt64

newtype Money = Money Rational
  deriving (Eq, Ord, Enum, Num, Real, Fractional, RealFrac)

instance Show Money where
  show (Money n) =
    let cents = round (n * 100) :: Int
    in case reverse (show cents) of
       c:d:whole -> reverse (c:d:'.':whole)
       somethingElse -> reverse somethingElse

instance Read Money where
  readsPrec _ input = do
    (n, rest) <- readFloat input
    return $ (Money n, rest)

instance PersistField Money where
  toPersistValue (Money n) = PersistRational n
  fromPersistValue (PersistRational n) = Right (Money n)
  fromPersistValue _ = Left "Money requires an int column type"

instance PersistFieldSql Money where
  sqlType _ = SqlNumeric 7 2

