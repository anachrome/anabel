module IRC.Parser (parseMessage) where

import Control.Applicative ((<$>), (<*), (<*>), (*>))

import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser) -- for type Parser = Parsec String ()

import IRC.Types

-- doing my best to be complient with rfc2812, but some of the parsers here are
-- a little liberal in terms of what they'll accept.
-- message format being parsed is described in section 2.3.1 of the rfc
-- some of which is excerpted in these comments

parseMessage :: String -> Either ParseError Message
parseMessage = parse message "IRC Parser"

-- message = [ ":" prefix SPACE ] command [ params ] crlf
message :: Parser Message
message = Message <$> optionMaybe prefix <*> word <*> args <* crlf

-- prefix = servname / (nickname [ [ "!" user ] "@" host ])
-- a prefix either represents a server or a nickname--the definitions of a
-- nick prefix and a server prefix overlap, so in the ambiguous case we default
-- to a nickname.  if this ends up being wrong, it is possible to convert
-- between nick and server prefixes--see IRC.Types for more details
prefix :: Parser Prefix
prefix = char ':' *> (try serverPrefix <|> nickPrefix)

-- a server can be either a hostname, ipv4 address, or ipv6 address.
-- in the former two cases, we'd assume a form of "x.x.x [...]", in the latter,
-- "x:x:x [...]".  while *technically* there could be a server name that
-- doesn't fall into one of those patterns, such a form would be impossible to
-- disambiguate from a nickname form, which is our default.
-- ...so basically we just say "got a '.' or a ':'?  server!"
-- (to prevent the host-portion of a nickname prefix tricking the parser, we
-- halt parsing on ! or @)
serverPrefix :: Parser Prefix
serverPrefix = (Server .) . (++)
    <$> manyTill (noneOf " !@") (lookAhead $ oneOf ".:")
    <*> word

-- if we've decide we've got a nick prefix (using only the most thorough and
-- rigorous process), we have to parse that into nick and optional user and host
-- portions.  (while technically, host can only appear if user also appears, we
-- take the more liberal option--since '@' is an illegal nick character anyway)
nickPrefix :: Parser Prefix
nickPrefix = User
    <$> wordBy "!@"
    <*> optionMaybe (char '!' *> wordBy "@")
    <*> optionMaybe (char '@' *> word)

-- the format of the argslist is a series of words, ending with an (optional)
-- single trailing arg (which may contain spaces) signified by a colon.
-- *technically*, there can be a max of 14 args--and any after that are implied
-- to be part of the fourteenth trailing arg.  we're ignoring this limit because
-- it's arbitrary and rarely relevant in practice
args :: Parser [String]
args = many $ (char ':' *> many msgChar) <|> word

--
-- these last parsers are just utility parsers
--

-- a word is basically a string of things that aren't spaces
-- because we end up wanting to eat spaces wherever a word parser appears, the
-- parser eats the spaces for us.
-- word is represented as a specific form of wordBy, which allows additional
-- arbitrary boundary characters
word :: Parser String
word = wordBy ""

wordBy :: [Char] -> Parser String
wordBy cs = many1 (noneOf $ " \r\n" ++ cs) <* spaces

-- for our purposes, spaces (as term separators) can only be actual spaces
spaces :: Parser String
spaces = many $ char ' '

-- anyChar limited to the scope of an irc message
-- (technically \NUL should be here as well, but there's no real reason to
-- exclude it--the rfc notes that the only reason it isn't allowed is difficulty
-- in cstring processing.  we're not using cstrings, so we don't care)
msgChar :: Parser Char
msgChar = noneOf "\r\n"

-- crlf just eats all the newline-type things at the end.
-- all of them.
crlf :: Parser String
crlf = many $ oneOf "\r\n"

