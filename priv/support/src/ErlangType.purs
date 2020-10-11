module Erlang.Type where

import Prelude
import Node.Buffer (Buffer, toArray, fromArray, toArray, concat)
import Data.List (List)
import Data.BigInt as BI
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (error, throwException)

data ErlangTerm =
    ErlangNum (BI.BigInt)
    | ErlangString (String)
    | ErlangList (List ErlangTerm)
    | ErlangBinary (Buffer)
    | ErlangTuple (Array ErlangTerm)

instance showErlangTerm :: Show ErlangTerm where
    show (ErlangNum a) =
        show $ BI.toString a
    show (ErlangList a) =
        show a
    show (ErlangBinary a) =
        show $ unsafePerformEffect $ toArray a
    show (ErlangString a) =
        show a
    show (ErlangTuple a) =
        show a

instance eqErlangTerm :: Eq ErlangTerm where
    eq (ErlangNum a) (ErlangNum b) = a == b
    eq (ErlangString a) (ErlangString b) = a == b
    eq (ErlangList a) (ErlangList b) = a == b
    eq (ErlangBinary a) (ErlangBinary b) = (unsafePerformEffect $ toArray a) == (unsafePerformEffect $ toArray b)
    eq (ErlangTuple a) (ErlangTuple b) = a == b
    eq _ _ = false

concatArrays :: Buffer -> Buffer -> Effect (Buffer)
concatArrays a b = do
    concat [a, b]

instance semigroupErlangTerm :: Semigroup ErlangTerm where
     append (ErlangString a) (ErlangString b) = ErlangString $ a <> b
     append (ErlangBinary a) (ErlangBinary b) = ErlangBinary $ unsafePerformEffect (concatArrays a b)
     append _ _ = unsafePerformEffect $ throwException $ error $ "Invalid append"

