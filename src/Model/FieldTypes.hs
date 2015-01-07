{-# LANGUAGE TemplateHaskell #-}
module Model.FieldTypes where

import Data.Bits
import Data.Int

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

zeroBits :: Bits a => a
zeroBits = clearBit (bit 0) 0

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

