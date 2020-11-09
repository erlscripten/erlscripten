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

type ErlangFun = Partial => Array ErlangTerm -> Effect ErlangTerm

-- TODO: add floats
data ErlangTerm
    = ErlangNum       Int
    | ErlangAtom      String
    | ErlangCons      ErlangTerm ErlangTerm
    | ErlangEmptyList
    | ErlangBinary    Buffer
    | ErlangTuple     (Array ErlangTerm)
    | ErlangFun       Int ErlangFun
    | ErlangMap       (Map.Map ErlangTerm ErlangTerm)

instance showErlangTerm :: Show ErlangTerm where
    show (ErlangNum a) =
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

instance eqErlangTerm :: Eq ErlangTerm where
    eq (ErlangAtom a) (ErlangAtom b) = a == b
    eq (ErlangNum a) (ErlangNum b) = a == b
    eq (ErlangCons ha ta) (ErlangCons hb tb) = ha == hb && ta == tb
    eq ErlangEmptyList ErlangEmptyList = true
    eq (ErlangBinary a) (ErlangBinary b) = (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
    eq (ErlangTuple a) (ErlangTuple b) = a == b
    eq (ErlangMap m1) (ErlangMap m2) = m1 == m2
    eq _ _ = false


instance ordErlangTerm :: Ord ErlangTerm where
    compare (ErlangNum a) (ErlangNum b) = compare a b
    compare (ErlangAtom a) (ErlangAtom b) = compare a b
    compare (ErlangCons ha ta) (ErlangCons hb tb) = compare [ha, ta] [hb, tb]
    compare ErlangEmptyList ErlangEmptyList = EQ
    compare (ErlangBinary a) (ErlangBinary b) =
      compare (unsafePerformEffect $ toArray a) (unsafePerformEffect $ toArray b)
    compare (ErlangTuple a) (ErlangTuple b) = compare a b
    compare (ErlangMap m1) (ErlangMap m2) =
      let sizeCMP = compare (Map.size m1) (Map.size m2)
      in case sizeCMP of
        EQ ->
          let l1 :: DL.List (DT.Tuple ErlangTerm ErlangTerm)
              l1 = Map.toUnfoldable m1
              l2 = Map.toUnfoldable m2 -- FIXME erlang float vs int ordering
          in compare l1 l2
        _ -> sizeCMP

    compare   (ErlangBinary _)  _ = GT
    compare   (ErlangCons _ _)  _ = GT
    compare   (ErlangEmptyList) _ = GT
    compare   (ErlangMap _)     _ = GT
    compare   (ErlangTuple _)   _ = GT
    compare   (ErlangFun _ _)   _ = GT
    compare   (ErlangAtom _)    _ = GT
    compare   (ErlangNum _)     _ = GT

    compare _ (ErlangNum _)       = LT
    compare _ (ErlangAtom _)      = LT
    compare _ (ErlangFun _ _)     = LT
    compare _ (ErlangTuple _)     = LT
    compare _ (ErlangMap _)       = LT
    compare _ (ErlangEmptyList)   = LT
    compare _ (ErlangCons _ _)    = LT
    compare _ (ErlangBinary _)    = LT


concatArrays :: Buffer -> Buffer -> Effect (Buffer)
concatArrays a b = do
    concat [a, b]

instance semigroupErlangTerm :: Semigroup ErlangTerm where
     append (ErlangBinary a) (ErlangBinary b) = ErlangBinary $ unsafePerformEffect (concatArrays a b)
     append _ _ = unsafePerformEffect $ throw "Invalid append"

erlangListToList :: ErlangTerm -> DM.Maybe (DL.List ErlangTerm)
erlangListToList ErlangEmptyList = DM.Just DL.Nil
erlangListToList (ErlangCons h t) | DM.Just et <- erlangListToList t = DM.Just (DL.Cons h et)
erlangListToList _ = DM.Nothing

arrayToErlangList :: Array ErlangTerm -> ErlangTerm
arrayToErlangList arr = go (DL.fromFoldable arr) where
  go DL.Nil = ErlangEmptyList
  go (DL.Cons h t) = ErlangCons h (go t)

erlangStringToString :: ErlangTerm -> DM.Maybe String
erlangStringToString term = DM.Nothing -- FIXME

boolToTerm :: Boolean -> ErlangTerm
boolToTerm true = ErlangAtom "true"
boolToTerm false = ErlangAtom "false"
