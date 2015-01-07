{-# LANGUAGE TemplateHaskell #-}
module Model.FieldTypes where

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
