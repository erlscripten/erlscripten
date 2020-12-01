module Erlang.Type where

import Prelude
import Node.Buffer (Buffer, toArray, fromArray, toArray, concat)
import Data.List as DL
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Map as Map
import Data.Char as DC
import Data.Tuple as DT
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)

type ErlangFun = Array ErlangTerm -> ErlangTerm

data ErlangTerm
    = ErlangInt       DBI.BigInt
    | ErlangFloat     Number
    | ErlangAtom      String
    | ErlangCons      ErlangTerm ErlangTerm
    | ErlangEmptyList
    | ErlangBinary    Buffer
    | ErlangTuple     (Array ErlangTerm)
    | ErlangFun       Int ErlangFun
    | ErlangMap       (Map.Map ErlangTerm ErlangTerm)
    | ErlangReference Int
    | ErlangPID       Int

instance showErlangTerm :: Show ErlangTerm where
    show (ErlangInt a) =
        show a
    show (ErlangFloat a) =
        show a
    show term  | DM.Just l <- erlangListToList term =
        show l
    show (ErlangCons h t) =
        "[" <> show h <> "|" <> show t <> "]"
    show ErlangEmptyList =
        "[]"
    show (ErlangBinary a) =
        show $ unsafePerformEffect $ toArray a
    show (ErlangTuple a) =
        show a
    show (ErlangFun arity _) =
        "<some_function/" <> show arity <> ">"
    show (ErlangAtom atom) =
        atom
    show (ErlangMap m) =
        show m
    show (ErlangReference a) =
        show a
    show (ErlangPID a) =
        show a

eqErlangTermImpl :: ErlangTerm -> ErlangTerm -> Boolean
eqErlangTermImpl (ErlangAtom a) (ErlangAtom b) = a == b
eqErlangTermImpl (ErlangInt a) (ErlangInt b) = a == b
eqErlangTermImpl (ErlangFloat a) (ErlangFloat b) = a == b
eqErlangTermImpl (ErlangReference a) (ErlangReference b) = a == b
eqErlangTermImpl (ErlangPID a) (ErlangPID b) = a == b
eqErlangTermImpl (ErlangCons ha ta) (ErlangCons hb tb) =
  -- heads MUST NOT be compared by recursive call
  if eq ha hb then eqErlangTermImpl ta tb else false
eqErlangTermImpl ErlangEmptyList ErlangEmptyList = true
eqErlangTermImpl (ErlangBinary a) (ErlangBinary b) =
  (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
eqErlangTermImpl (ErlangTuple a) (ErlangTuple b) = a == b
eqErlangTermImpl (ErlangMap m1) (ErlangMap m2) = m1 == m2
eqErlangTermImpl _ _ = false

instance eqErlangTerm :: Eq ErlangTerm where
    eq = eqErlangTermImpl

compareErlangTermImpl :: ErlangTerm -> ErlangTerm -> Ordering
compareErlangTermImpl (ErlangInt a) (ErlangInt b) = compare a b
compareErlangTermImpl (ErlangFloat a) (ErlangFloat b) = compare a b
compareErlangTermImpl (ErlangReference a) (ErlangReference b) = compare a b
compareErlangTermImpl (ErlangPID a) (ErlangPID b) = compare a b
compareErlangTermImpl (ErlangAtom a) (ErlangAtom b) = compare a b
compareErlangTermImpl (ErlangCons ha ta) (ErlangCons hb tb) =
  -- heads MUST NOT be compared by recursive call
  case compare ha hb of
    EQ -> compareErlangTermImpl ta tb
    res -> res
compareErlangTermImpl ErlangEmptyList ErlangEmptyList = EQ
compareErlangTermImpl (ErlangBinary a) (ErlangBinary b) =
  compare (unsafePerformEffect $ toArray a) (unsafePerformEffect $ toArray b)
compareErlangTermImpl (ErlangTuple a) (ErlangTuple b) = compare a b
compareErlangTermImpl (ErlangMap m1) (ErlangMap m2) =
  let sizeCMP = compare (Map.size m1) (Map.size m2)
  in case sizeCMP of
    EQ ->
      let l1 :: DL.List (DT.Tuple ErlangTerm ErlangTerm)
          l1 = Map.toUnfoldable m1
          l2 = Map.toUnfoldable m2
      in compare l1 l2
    _ -> sizeCMP

compareErlangTermImpl   (ErlangBinary _)    _ = GT
compareErlangTermImpl   (ErlangCons _ _)    _ = GT
compareErlangTermImpl   (ErlangEmptyList)   _ = GT
compareErlangTermImpl   (ErlangMap _)       _ = GT
compareErlangTermImpl   (ErlangTuple _)     _ = GT
compareErlangTermImpl   (ErlangFun _ _)     _ = GT
compareErlangTermImpl   (ErlangAtom _)      _ = GT
compareErlangTermImpl   (ErlangPID _)       _ = LT
compareErlangTermImpl   (ErlangReference _) _ = LT
compareErlangTermImpl   (ErlangFloat _)     _ = LT
compareErlangTermImpl   (ErlangInt _)       _ = GT

compareErlangTermImpl _ (ErlangInt _)         = LT
compareErlangTermImpl _ (ErlangFloat _)       = LT
compareErlangTermImpl _ (ErlangReference _)   = LT
compareErlangTermImpl _ (ErlangPID _)         = LT
compareErlangTermImpl _ (ErlangAtom _)        = LT
compareErlangTermImpl _ (ErlangFun _ _)       = LT
compareErlangTermImpl _ (ErlangTuple _)       = LT
compareErlangTermImpl _ (ErlangMap _)         = LT
compareErlangTermImpl _ (ErlangEmptyList)     = LT
compareErlangTermImpl _ (ErlangCons _ _)      = LT
compareErlangTermImpl _ (ErlangBinary _)      = LT

instance ordErlangTerm :: Ord ErlangTerm where
    compare = compareErlangTermImpl


concatArrays :: Buffer -> Buffer -> Effect (Buffer)
concatArrays a b = concat [a, b]

instance semigroupErlangTerm :: Semigroup ErlangTerm where
     append (ErlangBinary a) (ErlangBinary b) = ErlangBinary $ unsafePerformEffect (concatArrays a b)
     append _ _ = unsafePerformEffect $ throw "Invalid append"


erlangListToList :: ErlangTerm -> DM.Maybe (DL.List ErlangTerm)
erlangListToList = go DL.Nil where
  go acc ErlangEmptyList = DM.Just (DL.reverse acc)
  go acc (ErlangCons h t) = go (DL.Cons h acc) t
  go _ _ = DM.Nothing

arrayToErlangList :: Array ErlangTerm -> ErlangTerm
arrayToErlangList arr = go (DL.fromFoldable arr) where
  go DL.Nil = ErlangEmptyList
  go (DL.Cons h t) = ErlangCons h (go t)

boolToTerm :: Boolean -> ErlangTerm
boolToTerm true = ErlangAtom "true"
boolToTerm false = ErlangAtom "false"
