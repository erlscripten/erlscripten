module Erlang.Binary where

import Prelude
import Erlang.Type (ErlangTerm(ErlangBinary, ErlangNum, ErlangTuple))
import Node.Buffer as Buffer
-- import Node.Buffer.Unsafe as Buffer
import Node.Buffer(Buffer)
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

data Endian = Big | Little
data Sign   = Signed | Unsigned
data BinResult = Nah | Ok ErlangTerm Buffer.Buffer

fromFoldable :: forall f. Foldable f => f Int -> Buffer
fromFoldable f = unsafePerformEffect (Buffer.fromArray (DA.fromFoldable f))

concat :: Array Buffer -> Buffer
concat args = unsafePerformEffect $ Buffer.concat args

buffer (ErlangBinary x) = x
buffer _ = error "buffer – not a binary"

length (ErlangBinary x) = unsafePerformEffect $ Buffer.size x
length _ = error "length – not a binary"

unboxed_byte_size :: Buffer.Buffer -> Int
unboxed_byte_size b = unsafePerformEffect $ Buffer.size b

empty :: Buffer.Buffer -> Boolean
empty buf = unsafePerformEffect $ map (_ == 0) (Buffer.size buf)

size :: Buffer -> ErlangTerm
size = ErlangNum <<< unsafePerformEffect <<< Buffer.size

chop_int :: Buffer.Buffer -> Int -> Int -> Endian -> Sign -> BinResult
chop_int buf size unit endian sign = unsafePerformEffect $ do
  let chopSize = size * unit `div` 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
    pure $ Ok (
      case endian of
        Big    -> ErlangNum (decode_unsigned_big chop)
        Little -> ErlangNum (decode_unsigned_little chop)
      ) rest

chop_bin :: Buffer.Buffer -> Int -> Int -> BinResult
chop_bin buf size unit = unsafePerformEffect $ do
  let chopSize = size * unit `div` 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
    pure $ Ok (ErlangBinary chop) rest

unsafe_at :: Buffer -> Int -> Int
unsafe_at buf n = unsafePartial $ fromJust $ unsafePerformEffect $ (Buffer.getAtOffset n buf)

decode_unsigned_big :: Buffer -> Int
decode_unsigned_big buf = unsafePerformEffect (Buffer.size buf >>= go buf 0) where
  go :: Buffer -> Int -> Int -> Effect Int
  go buf acc size = do
    case size of
      0 -> pure acc
      _ -> go
           (Buffer.slice 1 size buf)
           (256 * acc + unsafe_at buf 0)
           (size - 1)

decode_unsigned_little :: Buffer -> Int
decode_unsigned_little buf = unsafePerformEffect (Buffer.size buf >>= go buf 0) where
  go :: Buffer -> Int -> Int -> Effect Int
  go buf acc size = do
    case size of
      0 -> pure acc
      _ -> go
           (Buffer.slice 0 (size - 1) buf)
           (256 * acc + unsafe_at buf (size - 1))
           (size - 1)

from_int :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Sign -> Buffer
from_int n size unit endian sign = fromFoldable []  -- TODO

format_bin :: ErlangTerm -> ErlangTerm -> Int -> Buffer
format_bin buf size unit = fromFoldable []  -- TODO

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
