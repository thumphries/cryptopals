{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Set1 where


import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char (chr, ord, toLower)
import           Data.Foldable (maximumBy)
import           Data.List ((!!), iterate)
import           Data.Word (Word8)

import           P

import           System.IO (IO)

import qualified Prelude


test :: IO ByteString
test = B.readFile "data/6.txt"

-- For binary strings a and b the Hamming distance is equal to the
-- number of ones (population count) in a XOR b.
-- FIX length mismatch case is not handled
hammingDistance :: ByteString -> ByteString -> Int
hammingDistance a = sum . fmap popCount . B.zipWith xor a

-- -----------------------------------------------------------------------------

repeatingKeyXor :: ByteString -> ByteString -> ByteString
repeatingKeyXor key plain = B.pack (B.zipWith xor keyrep plain)
  where
    infkey = iterate (B.append key) B.empty !! B.length plain
    keyrep = B.take (B.length plain) infkey


-- | Given a list of ciphertext, pick the one most likely to have been
-- singleByteXor'd, and spit out its deciphered value
detectSingleByteXor :: [ByteString] -> ByteString
detectSingleByteXor = snd . maximumBy (compare `on` fst) . brute
  where
    brute = fmap unSingleByteXor

-- | Brute-force the key to a string that's been fixedXor'd.
-- Uses ASCII character frequency to figure out the best option.
unSingleByteXor :: ByteString -> (Integer, ByteString)
unSingleByteXor bs = maximumBy (compare `on` fst) . fmap (prospect . ($bs)) $ candidates
  where
    candidates = fmap (B.map . xor) [0..255 :: Word8]
    --
    prospect :: ByteString -> (Integer, ByteString)
    prospect bs = (foldl' countAscii 0 (B.unpack bs), bs)
    --
    countAscii n b
      -- b `elem` ['A'..'Z', 'a'..'z']
      | (b >= 65 && b <= 90) || (b >= 97 && b <= 122) = n + 1
      | b == 32 = n + 1
      | otherwise = n

-- | As long as the two bytestrings are the same length, return their
-- xor combination.
fixedXor :: ByteString -> ByteString -> Maybe ByteString
fixedXor a b
  | B.length a /= B.length b = Nothing
  | otherwise = Just . B.pack $ B.zipWith xor a b


-- -----------------------------------------------------------------------------
-- Hex

fromHex :: [Char] -> Maybe ByteString
fromHex = liftM B.pack . unHexPairs
  where
    unHexPairs :: [Char] -> Maybe [Word8]
    unHexPairs ls = case ls of
      (x:y:ys) -> do
        w <- unHexPair x y
        liftM (w:) (unHexPairs ys)
      (x:[]) -> do
        w <- unHexChar x
        pure [w]
      [] -> pure []

unHexChar :: Char -> Maybe Word8
unHexChar (toLower -> c)
  | c >= '0' && c <= '9' = Just . fromIntegral $ ord c - ord '0'
  | c >= 'a' && c <= 'f' = Just . fromIntegral $ ord c - ord 'a' + 10
  | otherwise = Nothing

hexChar :: Word8 -> Maybe Char
hexChar b
  | b >= 0  && b <= 9  = Just (chr (fromIntegral b + ord '0'))
  | b >= 10 && b <= 15 = Just (chr (fromIntegral b + ord 'a' - 10))
  | otherwise = Nothing

hexPair :: Word8 -> (Char, Char)
hexPair a = (b, c)
  where
    b = fromMaybe uh $ hexChar (a `shiftR` 4)
    c = fromMaybe uh $ hexChar (a .&. 15)
    uh = Prelude.error "hexPair: bad bitmask, uh oh"

unHexPair :: Char -> Char -> Maybe Word8
unHexPair l' r' = do
  l <- unHexChar l'
  r <- unHexChar r'
  pure $ (l `shiftL` 4) .|. r

toHex :: ByteString -> [Char]
toHex = hexChars . B.unpack
  where
    hexChars ls = case ls of
      (b:bs) -> let (c, d) = hexPair b in c : d: hexChars bs
      [] -> []

-- -----------------------------------------------------------------------------
-- Base64

toBase64 :: ByteString -> [Char]
toBase64 = unBase64Triples . B.unpack
  where
    unBase64Triples :: [Word8] -> [Char]
    unBase64Triples [] = []
    unBase64Triples ls =
      let ((c1, c2, c3, c4), zs) = case ls of
            (x:y:z:zs) -> (unBase64Triple x y z, zs)
            (x:y:[])   -> (unBase64Triple x y 0, [])
            (x:[])     -> (unBase64Triple x 0 0, [])
            _          -> (unBase64Triple 0 0 0, [])
      in c1 : c2 : c3 : c4 : unBase64Triples zs

unBase64Triple :: Word8 -> Word8 -> Word8 -> (Char, Char, Char, Char)
unBase64Triple w1' w2' w3' = fromMaybe uh $ do
    c1 <- unBase64Sextet w1
    c2 <- unBase64Sextet w2
    c3 <- unBase64Sextet w3
    c4 <- unBase64Sextet w4
    pure (c1, c2, c3, c4)
  where
    w1 = w1' `shiftR` 2
    w2 = sextet $ (w1' `shiftL` 4) .|. (w2' `shiftR` 4)
    w3 = sextet $ (w2' `shiftL` 2) .|. (w3' `shiftR` 6)
    w4 = sextet w3'
    uh = Prelude.error "unBase64Triple: bad bitmask, uh oh"

unBase64Sextet :: Word8 -> Maybe Char
unBase64Sextet w
  | w >= 0  && w <= 25 = Just . chr $ fromIntegral w + (ord 'A')
  | w >= 26 && w <= 51 = Just . chr $ fromIntegral w - 26 + (ord 'a')
  | w >= 52 && w <= 61 = Just . chr $ fromIntegral w - 52 + (ord '0')
  | w == 62 = Just '+'
  | w == 63 = Just '/'
  | otherwise = Nothing

sextet :: Word8 -> Word8
sextet b = b .&. complement 192

unBase64Chars :: Char -> Char -> Char -> Char -> Maybe (Word8, Word8, Word8)
unBase64Chars w x y z = do
  s1 <- unBase64Char w
  traceM (show s1)
  s2 <- unBase64Char x
  traceM (show s2)
  s3 <- unBase64Char y
  traceM (show s3)
  s4 <- unBase64Char z
  traceM (show s4)
  pure (w1 s1 s2, w2 s2 s3, w3 s3 s4)
  where
    w1 s1 s2 = (s1 `shiftL` 2) .|. (s2 `shiftR` 4)
    w2 s2 s3 = (s2 `shiftL` 4) .|. (s3 `shiftR` 4)
    w3 s3 s4 = (s3 `shiftL` 6) .|. s4

unBase64Char :: Char -> Maybe Word8
unBase64Char c
  | c >= 'A' && c <= 'Z' = Just $ fromIntegral (ord c - ord 'A')
  | c >= 'a' && c <= 'z' = Just $ fromIntegral (ord c - ord 'a' + 26)
  | c >= '0' && c <= '9' = Just $ fromIntegral (ord c - ord '0' + 52)
  | c == '+' = Just 62
  | c == '/' = Just 63
  | otherwise = Nothing

-- FIX need to implement == padding
fromBase64 :: [Char] -> Maybe ByteString
fromBase64 = liftM B.pack . parse
  where
    j = '\0'
    recur :: [Char] -> (Word8, Word8, Word8) -> Maybe [Word8]
    recur xs (a, b, c) = liftM ([a,b,c] <>) (parse xs)
    parse :: [Char] -> Maybe [Word8]
    parse ls = case ls of
      (w:x:y:z:xs) -> unBase64Chars w x y z >>= recur xs
      (x:y:z:[])   -> unBase64Chars x y z j >>= recur []
      (x:y:[])     -> unBase64Chars x y j j >>= recur []
      (x:[])       -> unBase64Chars x j j j >>= recur []
      []           -> pure []
