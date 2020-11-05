module Erlang.Binary where

import Prelude
import Erlang.Type (ErlangTerm(ErlangBinary, ErlangNum, ErlangTuple))
import Node.Buffer as Buffer
import Node.Encoding
import Data.BigInt as BI
import Data.Num (class Num, fromBigInt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (error, throwException)
import Data.UInt (UInt, toInt, fromInt)
import Data.Array.NonEmpty as NonEmpty
import Partial.Unsafe (unsafePartial)
import Data.Base58 as B58
import Data.Maybe(Maybe, fromJust)

-- buffer (ErlangBinary x) = x

-- length (ErlangBinary x) = unsafePerformEffect $ Buffer.size x
-- length _ = unsafePerformEffect $ throwException $ error $ "bad value"

-- at (ErlangBinary a) n = unsafePartial $ fromJust $ unsafePerformEffect $ (Buffer.getAtOffset n a)
-- at _ _ = unsafePerformEffect $ throwException $ error $ "bad value"

-- empty :: ErlangTerm
-- empty = ErlangBinary $ unsafePerformEffect $ Buffer.create 0

-- firstN (ErlangBinary a) num =
--     ErlangTuple [ErlangBinary $ Buffer.slice 0 num a, ErlangBinary $ Buffer.slice num (unsafePerformEffect $ Buffer.size a) a]

-- firstNNum a@(ErlangBinary _) num =
--     let
--         ErlangTuple [b@(ErlangBinary _), rest] = firstN a num
--     in
--         ErlangTuple [decode_unsigned b, rest]

-- decode_unsigned :: Partial => ErlangTerm -> ErlangTerm
-- decode_unsigned (ErlangBinary a) | (unsafePerformEffect $ Buffer.size a) == 0 =
--     ErlangNum (BI.fromInt 0)
-- decode_unsigned a@(ErlangBinary _) =
--     let
--         ErlangTuple [h, t] = firstN a 1
--         ErlangNum n = decode_unsigned t
--     in
--         ErlangNum $ ((BI.fromInt $ at h 0) + (BI.fromInt 256) * n)

-- toArray :: Partial => ErlangTerm -> Effect (Array Int)
-- toArray (ErlangBinary a) = do
--     Buffer.toArray a

-- toB64 :: Partial => ErlangTerm -> String
-- toB64 (ErlangBinary a) =
--     unsafePerformEffect $ Buffer.toString Base64 a
-- fromB64 :: String -> ErlangTerm
-- fromB64 str = do
--     ErlangBinary $ unsafePerformEffect $ Buffer.fromString str Base64

-- toB58 :: Partial => ErlangTerm -> String
-- toB58 (ErlangBinary a) =
--     B58.encode $ unsafePerformEffect $ Buffer.toArray a
-- fromB58 :: String -> Maybe ErlangTerm
-- fromB58 str = do
--     s <- B58.decode str
--     pure $ ErlangBinary $ unsafePerformEffect $ Buffer.fromArray s

-- class BinaryEncoder a where
--     encode_unsigned :: a -> Effect ErlangTerm
--     fromArray :: Array a -> Effect ErlangTerm

-- instance bigIntBinaryEncoder :: BinaryEncoder BI.BigInt where
--     encode_unsigned x =
--         case BI.digitsInBase 256 x of
--             {isNegative: true} ->
--                 throwException $ error "Unable to encode negative number as binary"
--             {value: digits} -> do
--                 a <- Buffer.fromArray $ NonEmpty.toArray digits
--                 pure $ ErlangBinary a

--     fromArray a = fromArray $ ((map fromBigInt a) :: Array Int)

-- instance erlangTermBinaryEncoder :: BinaryEncoder ErlangTerm where
--     encode_unsigned (ErlangNum x) =
--         encode_unsigned x
--     encode_unsigned term =
--         throwException $ error $ "Unable to encode " <> (show term)

--     fromArray term =
--         throwException $ error $ "Unable to encode " <> (show term)

-- instance uIntBinaryEncoder :: BinaryEncoder UInt where
--     encode_unsigned x =
--         encode_unsigned $ toInt $ x

--     fromArray a = do
--         arr <- Buffer.fromArray $ map toInt a
--         pure $ ErlangBinary arr

-- instance intBinaryEncoder :: BinaryEncoder Int where
--     encode_unsigned x =
--         encode_unsigned $ BI.fromInt x

--     fromArray a = fromArray (map fromInt a)
