{- Git pkt-line format, as documented in 
 - git/Documentation/technical/protocol-common.txt
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Protocol.PktLine (
	PktLine,
	flushPkt,
	stringPktLine,
	pktLineString,
	streamPktLine,
	encodePktLine,
	parsePktLine,
	decodePktLine,
	splitPktLine,
	readPktLine,
	writePktLine,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Lazy as AL
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import System.IO

import Utility.PartialPrelude
import Utility.FileSystemEncoding

-- | A pkt-line encodes a variable length binary string with a maximum size.
--
-- This module only exports smart constructors for legal pkt-lines.
newtype PktLine = PktLine S.ByteString
	deriving (Show, Eq)

-- | Maximum data that can be contained in a pkt-line, not
-- including the 4 byte length header.
maxPktLineContent :: Word16
maxPktLineContent = 65516

-- | Maximum length of a pkt-line, including the 4 byte size header.
maxPktLineLength :: Word16
maxPktLineLength = 65520

-- | The flush-pkt, a special case in the protocol that is often used to
-- eg, signal the end of a stream of binary data.
flushPkt :: PktLine
flushPkt = PktLine S.empty

-- | Encodes a String as a PktLine. Fails if the String is too large.
--
-- A trailing newline is included after it, as the protocol recommends
-- doing for non-binary data.
stringPktLine :: String -> Maybe PktLine
stringPktLine s = 
	let b = encodeBSS s <> "\n"
	in if S.length b > fromIntegral maxPktLineContent
		then Nothing
		else Just (PktLine b)

-- | Extracts a String from a PktLine. Any trailing newline is removed.
pktLineString :: PktLine -> String
pktLineString (PktLine b) = 
	let s = decodeBSS b
	in if end s == "\n"
		then beginning s
		else s

-- | Creates a stream of PktLines encoding a lazy ByteString of any size.
-- Note that the stream is not terminated with a flushPkt.
streamPktLine :: L.ByteString -> [PktLine]
streamPktLine l =
	let (chunk, rest) = L.splitAt (fromIntegral maxPktLineContent) l
	    pktline = PktLine $ mconcat $ L.toChunks chunk
	in if L.null rest
		then pktline : []
		else pktline : streamPktLine rest

-- | ByteString builder for a pkt-line.
encodePktLine :: PktLine -> Builder
encodePktLine (PktLine b)
	-- Avoid sending an empty pkt-line; send a flush-pkt instead.
	| S.null b = "0000"
	| otherwise = lengthheader <> byteString b
  where
	-- The length header is always 4 bytes long, and includes
	-- itself in its length.
	lengthheader = word16HexFixed (fromIntegral (S.length b) + 4)

-- | Attoparsec parser for a pkt-line.
parsePktLine :: Parser PktLine
parsePktLine = parsePktLine' =<< parseLengthHeader

parsePktLine' :: Int -> Parser PktLine
parsePktLine' len
	| len == 0 = do
		endOfInput
		return flushPkt
	-- It's impossible for a pkt-line to be less than
	-- 4 bytes long, since the length header is 4 bytes.
	| len < 4 = fail "invalid pkt-line length"
	| otherwise = PktLine <$> A.take (len - 4)

parseLengthHeader :: Parser Int
parseLengthHeader = do
	-- Attoparsec's hexidecimal parser will consume any amount
	-- of hex, but the length header is limited to 4 bytes, so
	-- take those and parse only them.
	h <- A.take 4
	-- Require all 4 bytes to be hexidecimal by using endOfInput.
	case parseOnly (hexadecimal <* endOfInput) h of
		Left e -> fail e
		Right len
			| len > fromIntegral maxPktLineLength ->
				fail "pkt-line too long"
			| otherwise -> return len

-- | The ByteString must contain only a pkt-line with no additional data
-- or this will fail.
decodePktLine :: S.ByteString -> Either String PktLine
decodePktLine = parseOnly (parsePktLine <* endOfInput)

-- | Split the next PktLine from a lazy ByteString, returning it and the
-- remainder of the ByteString.
splitPktLine :: L.ByteString -> Either String (PktLine, L.ByteString)
splitPktLine = go . AL.parse parsePktLine
  where
	go (AL.Done rest p) = Right (p, rest)
	go (AL.Fail _ _ e) = Left e

-- | Read the next PktLine from a Handle. 
--
-- Nothing is returned at EOF.
readPktLine :: Handle -> IO (Maybe (Either String PktLine))
readPktLine h = do
	header <- S.hGet h 4
	if S.null header
		then return Nothing
		else case parseOnly parseLengthHeader header of
			Left e -> return $ Just $ Left e
			Right len
				| len == 0 -> return $ Just $ Right flushPkt
				| otherwise -> do
					-- The header parser rejects headers that
					-- are too long, so this will never use
					-- much memory.
					body <- S.hGet h (len - 4)
					let parser = parsePktLine' len <* endOfInput
					return $ Just $ parseOnly parser body

-- | Sends a packet to the Handle. Does not flush the Handle.
writePktLine :: Handle -> PktLine -> IO ()
writePktLine h = hPutBuilder h . encodePktLine
