module Erlang.Binary where

import Prelude
import Erlang.Type (ErlangTerm(ErlangBinary, ErlangNum, ErlangTuple))
import Node.Buffer as Buffer
import Node.Encoding
import Data.Num (class Num, fromBigInt)
import Data.BigInt as BI
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw, throwException)
import Data.UInt (UInt, toInt, fromInt)
import Data.Array.NonEmpty as NonEmpty
import Partial.Unsafe (unsafePartial)
import Data.Base58 as B58
import Data.Array as DA
import Data.Maybe(Maybe, fromJust)
import Data.Foldable

error :: forall a. String -> a
error = unsafePerformEffect <<< throw


fromFoldable :: forall f. Foldable f => f Int -> ErlangTerm
fromFoldable f = ErlangBinary (unsafePerformEffect (Buffer.fromArray (DA.fromFoldable f)))

concat :: Array ErlangTerm -> ErlangTerm
concat args =
  ErlangBinary (unsafePerformEffect $ Buffer.concat (map buffer args))

buffer (ErlangBinary x) = x
buffer _ = error "buffer – not a binary"

length (ErlangBinary x) = unsafePerformEffect $ Buffer.size x
length _ = error "length – not a binary"

unboxed_byte_size :: Buffer.Buffer -> Int
unboxed_byte_size b = unsafePerformEffect $ Buffer.size b

-- at (ErlangBinary a) n = unsafePartial $ fromJust $ unsafePerformEffect $ (Buffer.getAtOffset n a)
-- at _ _ = error "at – not a binary"

-- empty :: ErlangTerm
-- empty = ErlangBinary $ unsafePerformEffect $ Buffer.create 0

-- firstN (ErlangBinary a) num =
--     ErlangTuple [ErlangBinary $ Buffer.slice 0 num a, ErlangBinary $ Buffer.slice num (unsafePerformEffect $ Buffer.size a) a]
-- firstN _ = error "firstN – not a binary"

-- firstNNum a@(ErlangBinary _) num =
--     let
--         ErlangTuple [b@(ErlangBinary _), rest] = firstN a num
--     in
--         ErlangTuple [decode_unsigned b, rest]
-- firstNNum _ = error "firstNNum – not a binary"

-- decode_unsigned :: ErlangTerm -> ErlangTerm
-- decode_unsigned (ErlangBinary a) | (unsafePerformEffect $ Buffer.size a) == 0 =
--     ErlangNum 0
-- decode_unsigned a@(ErlangBinary _) =
--     let
--         ErlangNum n = decode_unsigned ErlangBinary $ Buffer.slice num (unsafePerformEffect $ Buffer.size a) a
--     in
--         ErlangNum $ at (ErlangBinary $ Buffer.slice 0 num a) 0 + 256 * n
-- decode_unsigned _ = error "decode_unsigned – not a binary"

-- toArray :: ErlangTerm -> Effect (Array Int)
-- toArray (ErlangBinary a) = do
--     Buffer.toArray a
-- toArray _ = error "toArray – not a binary"


-- toB64 :: ErlangTerm -> String
-- toB64 (ErlangBinary a) =
--     unsafePerformEffect $ Buffer.toString Base64 a
-- toB64 _ = error "toB64 – not a binary"

-- fromB64 :: String -> ErlangTerm
-- fromB64 str = do
--     ErlangBinary $ unsafePerformEffect $ Buffer.fromString str Base64

-- toB58 :: Partial => ErlangTerm -> String
-- toB58 (ErlangBinary a) =
--     B58.encode $ unsafePerformEffect $ Buffer.toArray a
-- toB58 _ = error "toB58 – not a binary"

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
--         encode_unsigned (BI.fromInt x)

--     fromArray a = fromArray (map fromInt a)
