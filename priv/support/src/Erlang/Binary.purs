module Erlang.Binary where

import Prelude
import Erlang.Type
import Erlang.Exception as EXC
import Node.Buffer as Buffer
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
import Data.Maybe as DM
import Data.List as DL
import Data.Int as Int
import Data.Maybe(Maybe, fromJust)
import Data.Foldable

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

data Endian = Big | Little
data Sign   = Signed | Unsigned
data BinResult = Nah | Ok ErlangTerm Buffer.Buffer

fromFoldable :: forall f. Foldable f => f Int -> Buffer
fromFoldable f = unsafePerformEffect (Buffer.fromArray (DA.fromFoldable f))

buffer :: ErlangTerm -> Buffer
buffer (ErlangBinary buf) = buf
buffer _ = error "buffer: not a binary"

concat :: Array Buffer -> Buffer
concat args = unsafePerformEffect $ Buffer.concat args

concat_erl :: ErlangTerm -> ErlangTerm
concat_erl listTerm =
  case erlangListToList listTerm of
    DM.Nothing -> EXC.bad_generator listTerm
    DM.Just l -> ErlangBinary $ concat (DA.fromFoldable $ map buffer l)

empty :: Buffer.Buffer -> Boolean
empty buf = unsafePerformEffect $ map (_ == 0) (Buffer.size buf)

size :: Buffer -> ErlangTerm
size = ErlangInt <<< unsafePerformEffect <<< Buffer.size

packed_size :: ErlangTerm -> ErlangTerm
packed_size (ErlangBinary b) = size b
packed_size _ = EXC.badarg unit

chop_int :: Buffer.Buffer -> Int -> Int -> Endian -> Sign -> BinResult
chop_int buf size unit endian sign = unsafePerformEffect $ do
  let chopSize = (size * unit) / 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
        nonSign = case endian of
          Big    ->  (decode_unsigned_big chop)
          Little ->  (decode_unsigned_little chop)
        regSign = case sign of
          Unsigned -> nonSign
          Signed ->
            let p = Int.pow 2 (chopSize * 8 - 1)
            in if nonSign < p then nonSign else nonSign - p * 2
    pure $ Ok (ErlangInt regSign) rest

chop_bin :: Buffer.Buffer -> Int -> Int -> BinResult
chop_bin buf size unit = unsafePerformEffect $ do
  let chopSize = (size * unit) / 8
  size <- Buffer.size buf
  if size < chopSize
    then pure Nah
    else do
    let chop = Buffer.slice 0 chopSize buf
        rest = Buffer.slice chopSize size buf
    pure $ Ok (ErlangBinary chop) rest

foreign import arrayToFloat32 :: Array Int -> Number
foreign import arrayToFloat64 :: Array Int -> Number
chop_float :: Buffer.Buffer -> Int -> Int -> Endian -> BinResult
chop_float buf size unit endian = unsafePerformEffect $ do
  bufSize <- Buffer.size buf
  let chopSize = (size * unit) / 8
  if chopSize == 8 || chopSize == 4
    then do
      let chop = Buffer.slice 0 chopSize buf
          rest = Buffer.slice chopSize bufSize buf
      trueChop <- case endian of
        Big -> Buffer.toArray chop
        Little -> do
          asArr <- Buffer.toArray chop
          pure (DA.reverse asArr)
      pure $ Ok (if chopSize == 8
                 then ErlangFloat (arrayToFloat64 trueChop)
                 else ErlangFloat (arrayToFloat32 trueChop)
                ) rest
    else pure Nah

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

from_int :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
from_int (ErlangInt n) (ErlangInt size) unit endian =
  let bufSize = (size * unit) / 8
      build 0 _ acc = acc
      build x num acc = build (x - 1) (num / 256) (DL.Cons (num `mod` 256) acc)
      big = build bufSize n DL.Nil
  in fromFoldable $
    case endian of
      Big -> big
      Little -> DL.reverse big
from_int _ _ _ _ = EXC.badarg unit

foreign import float32ToArray :: Number -> Array Int
foreign import float64ToArray :: Number -> Array Int
from_float :: ErlangTerm -> ErlangTerm -> Int -> Endian -> Buffer
from_float _ (ErlangInt size) unit_ _ | size * unit_ /= 32 && size * unit_ /= 64 = EXC.badarg unit
from_float (ErlangInt i) s u e =
  from_float (ErlangFloat (Int.toNumber i)) s u e
from_float (ErlangFloat f) (ErlangInt size) unit_ endian =
  let big = case size * unit_ of
        32 -> float32ToArray f
        64 -> float64ToArray f
        _  -> error "shouldn't happen"
  in fromFoldable $ case endian of
    Little -> DA.reverse big
    Big -> big
from_float _ _ _ _ = EXC.badarg unit

format_bin :: ErlangTerm -> ErlangTerm -> Int -> Buffer
format_bin (ErlangBinary buf) (ErlangInt size) unit =
  let bufSize = size * unit / 8
  in Buffer.slice 0 bufSize buf
format_bin _ _ _ = EXC.badarg unit



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
