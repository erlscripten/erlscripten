module Erlang.Type where

import Prelude
import Node.Buffer (Buffer, toArray, fromArray, toArray, concat)
import Data.List as DL
import Data.Array as DA
import Data.BigInt as DBI
import Data.Maybe as DM
import Data.Char as DC
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (error, throwException)

type ErlangFun = Partial => Array ErlangTerm -> Effect ErlangTerm

-- TODO: add floats
data ErlangTerm
    = ErlangNum       DBI.BigInt
    | ErlangCons      ErlangTerm ErlangTerm
    | ErlangEmptyList
    | ErlangBinary    Buffer
    | ErlangTuple     (Array ErlangTerm)
    | ErlangFun       Int ErlangFun
    | ErlangAtom      String

instance showErlangTerm :: Show ErlangTerm where
    show (ErlangNum a) =
        show $ DBI.toString a
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
    show (ErlangAtom atom) = atom

instance eqErlangTerm :: Eq ErlangTerm where
    eq (ErlangNum a) (ErlangNum b) = a == b
    eq (ErlangCons ha ta) (ErlangCons hb tb) = ha == hb && ta == tb
    eq ErlangEmptyList ErlangEmptyList = true
    eq (ErlangBinary a) (ErlangBinary b) = (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
    eq (ErlangTuple a) (ErlangTuple b) = a == b
    eq _ _ = false

concatArrays :: Buffer -> Buffer -> Effect (Buffer)
concatArrays a b = do
    concat [a, b]

instance semigroupErlangTerm :: Semigroup ErlangTerm where
     append (ErlangBinary a) (ErlangBinary b) = ErlangBinary $ unsafePerformEffect (concatArrays a b)
     append _ _ = unsafePerformEffect $ throwException $ error $ "Invalid append"

erlangListToList :: ErlangTerm -> DM.Maybe (DL.List ErlangTerm)
erlangListToList ErlangEmptyList = DM.Just DL.Nil
erlangListToList (ErlangCons h t) | DM.Just et <- erlangListToList t = DM.Just (DL.Cons h et)
erlangListToList _ = DM.Nothing

erlangStringToString :: ErlangTerm -> DM.Maybe String
erlangStringToString term = DM.Nothing -- FIXME
