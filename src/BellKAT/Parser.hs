{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module BellKAT.Parser (
    parseOrderedGuardedPolicy,
    Parser
) where

import           Data.Functor
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)

import           Data.Default
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Control.Monad.Combinators.Expr (makeExprParser, Operator (..))

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests

-- | Parser type for policies, using 'Text' as input
type Parser = Parsec Void Text

-- | Main entry point for parsing an 'OrderedGuardedPolicy Char Action' from a 'String'.
-- It consumes all input, including leading/trailing whitespace.
parseOrderedGuardedPolicy
    :: (Default t, Tag t)
    => String
    -> Either (ParseErrorBundle Text Void) (OrderedGuardedPolicy (BoundedTest t) Action)
parseOrderedGuardedPolicy = parse policyParser "" . pack
  where
    -- Helper to consume leading/trailing spaces and ensure full input consumption
    policyParser = spaceConsumer *> pOrderedGuardedPolicy <* eof

-- * Lexer definitions
-- A set of common lexer functions for parsing expressions.

-- | Skips whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1                          -- (1) consume at least one space
    (L.skipLineComment "--")        -- (2) line comments starting with "--"
    (L.skipBlockComment "{-" "-}")  -- (3) block comments "{- ... -}"

-- | Lexeme parser: parses 'p' and then skips trailing whitespace/comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Symbol parser: parses 's' literally and then skips trailing whitespace/comments.
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Parses 'p' enclosed in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses a reserved keyword 'w'.
reserved :: Text -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- * Primitive parsers

-- | Parses a 'Location', which is represented as a string literal.
pLocation :: Parser Location
pLocation = Location <$> lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "location"

-- | Parses a pair of locations separated by a given symbol string, combining them with a provided function.
pBellPairSep :: (Location -> Location -> b) -> Text -> Parser b
pBellPairSep f separator = f <$> pLocation <* symbol separator <*> pLocation

-- | Parses a 'BellPair', e.g., 'A ~ B'.
pBellPair :: Parser (Location, Location)
pBellPair = pBellPairSep (,) "~"

-- | Parses an atomic BoundedTest guard term, which can be 'true', 'false',
-- an 'X /~ Y' expression, or a parenthesized BoundedTest.
pBoundedTestTerm :: (Default t, Tag t) => Parser (BoundedTest t)
pBoundedTestTerm =
        (true <$ reserved "true")
    <|> (false <$ reserved "false")
    <|> parens pBoundedTestGuard
    <|> boundedTestSingleton <$> pBellPairSep (~) "/~" <*> pure (rangeNotGreater 0)
    <?> "bound test term"

-- | Operator precedence table for 'BoundedTest'.
boundedTestOperatorTable :: (Default t, Tag t) => [[Operator Parser (BoundedTest t)]]
boundedTestOperatorTable =
    [ [ InfixL (symbol "||" $> (||*)) ] -- Logical OR (left-associative)
    ]

-- | Parses a 'BoundedTest' guard using an operator precedence table.
pBoundedTestGuard :: (Default t, Tag t) => Parser (BoundedTest t)
pBoundedTestGuard = makeExprParser pBoundedTestTerm boundedTestOperatorTable

-- * Action parsers

-- | Parses an 'Action'.
pAction :: Parser Action
pAction =
        (reserved "swap" >> parens (Swap <$> pLocation <* symbol "@" <*> pBellPair))
    <|> (reserved "trans" >> parens (Transmit <$> pLocation <* symbol "->" <*> pBellPair))
    <|> (reserved "distill" >> parens (Distill <$> pBellPair))
    <|> (reserved "create" >> parens (Create <$> pLocation))
    <|> (reserved "destroy" >> parens (Destroy <$> pBellPair))
    <|> (reserved "gen" >> parens (UnstableCreate <$> pBellPair))

-- * Policy parsers

-- | Parses a basic policy term, which can be an atomic action, 'one', a parenthesized policy,
-- an if-then-else statement, or a while loop.
pPolicyTerm :: (Default t, Tag t) => Parser (OrderedGuardedPolicy (BoundedTest t) Action)
pPolicyTerm =
        (OGPAtomic <$> pAction)
    <|> (OGPOne <$ reserved "one")
    <|> parens pOrderedGuardedPolicy
    <|> pIfThenElse
    <|> pWhile

-- | Parses an if-then-else policy: 'if <tag> then <policy1> else <policy2>'.
pIfThenElse :: (Default t, Tag t) => Parser (OrderedGuardedPolicy (BoundedTest t) Action)
pIfThenElse =
    OGPIfThenElse
        <$> (reserved "if" *> pBoundedTestGuard <* reserved "then")
        <*> (pOrderedGuardedPolicy <* reserved "else")
        <*> pOrderedGuardedPolicy

-- | Parses a while loop policy: 'while <tag> do <policy>'.
pWhile :: (Default t, Tag t) => Parser (OrderedGuardedPolicy (BoundedTest t) Action)
pWhile =
    OGPWhile
        <$> (reserved "while" *> pBoundedTestGuard <* reserved "do")
        <*> pOrderedGuardedPolicy

-- | Parses an 'OrderedGuardedPolicy Char Action' using an operator precedence table.
pOrderedGuardedPolicy :: (Default t, Tag t) => Parser (OrderedGuardedPolicy (BoundedTest t) Action)
pOrderedGuardedPolicy = makeExprParser pPolicyTerm operatorTable

-- | Operator precedence table for 'OrderedGuardedPolicy'.
-- The order of lists in the table defines precedence (lower index = lower precedence).
-- Operators within a list have the same precedence.
operatorTable :: (Default t, Tag t) => [[Operator Parser (OrderedGuardedPolicy (BoundedTest t) Action)]]
operatorTable =
    [ [ InfixN (symbol "<.>" $> OGPOrdered)   ] -- Ordered composition (non-associative)
    , [ InfixL (symbol "<||>" $> OGPParallel) ] -- Parallel composition (left-associative)
    , [ InfixL (symbol "<>" $> OGPSequence)   ] -- Sequential composition (left-associative)
    ]
