module Erlang.Type where

import Prelude
import Node.Buffer (Buffer, toArray, fromArray, toArray, concat)
import Data.List as DL
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Map as Map
import Data.Char as DC
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)

type ErlangFun = Partial => Array ErlangTerm -> Effect ErlangTerm

-- TODO: add floats
data ErlangTerm
    = ErlangNum       Int
    | ErlangCons      ErlangTerm ErlangTerm
    | ErlangEmptyList
    | ErlangBinary    Buffer
    | ErlangTuple     (Array ErlangTerm)
    | ErlangFun       Int ErlangFun
    | ErlangAtom      String
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
    eq (ErlangNum a) (ErlangNum b) = a == b
    eq (ErlangCons ha ta) (ErlangCons hb tb) = ha == hb && ta == tb
    eq ErlangEmptyList ErlangEmptyList = true
    eq (ErlangBinary a) (ErlangBinary b) = (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
    eq (ErlangTuple a) (ErlangTuple b) = a == b
    eq (ErlangMap m1) (ErlangMap m2) = m1 == m2
    eq _ _ = false


instance ordErlangTerm :: Ord ErlangTerm where
    compare (ErlangNum a) (ErlangNum b) = compare a b
    compare (ErlangCons ha ta) (ErlangCons hb tb) = compare [ha, ta] [hb, tb]
    compare ErlangEmptyList ErlangEmptyList = EQ
    compare (ErlangBinary a) (ErlangBinary b) =
      compare (unsafePerformEffect $ toArray a) (unsafePerformEffect $ toArray b)
    compare (ErlangTuple a) (ErlangTuple b) = compare a b
    compare (ErlangMap m1) (ErlangMap m2) = compare m1 m2

    compare   (ErlangNum _)     _ = GT
    compare _ (ErlangNum _)       = GT
    compare   (ErlangCons _ _)  _ = GT
    compare _ (ErlangCons _ _)    = GT
    compare   (ErlangEmptyList) _ = GT
    compare _ (ErlangEmptyList)   = GT
    compare   (ErlangBinary _)  _ = GT
    compare _ (ErlangBinary _)    = GT
    compare   (ErlangTuple _)   _ = GT
    compare _ (ErlangTuple _)     = GT
    compare   (ErlangMap _)     _ = GT
    compare _ (ErlangMap _)       = GT

    compare _ _ = unsafePerformEffect $ throw "illegal compare"

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

erlangStringToString :: ErlangTerm -> DM.Maybe String
erlangStringToString term = DM.Nothing -- FIXME
