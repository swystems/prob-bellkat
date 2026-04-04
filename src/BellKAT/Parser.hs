{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module BellKAT.Parser (
    parseSurfacePolicy,
    parseTest,
    Parser,
) where

import           Data.Functor
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)

import           Data.Default
import           Text.Megaparsec hiding (parseTest)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Control.Monad.Combinators.Expr (makeExprParser, Operator (..))

import           BellKAT.Definitions.Core
import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Definitions.Policy
import           BellKAT.Definitions.Tests
import           BellKAT.Parser.SurfacePolicy

-- | Parser type for policies, using 'Text' as input
type Parser = Parsec Void Text

-- | Main entry point for parsing a 'SurfacePolicy' from a 'String' coming from 'FilePath'.
-- It consumes all input, including leading/trailing whitespace, and then desugars it.
parseSurfacePolicy
    :: (Default t, Tag t)
    => FilePath  -- File name
    -> String    -- Input string
    -> Either (ParseErrorBundle Text Void) (SurfacePolicy t Action)
parseSurfacePolicy file = parse policyParser file . pack

parseTest
    :: (Default t, Tag t)
    => FilePath  -- File name
    -> String    -- Input string
    -> Either (ParseErrorBundle Text Void) (BooleanTest t)
parseTest file = parse pTestTerm file . pack 

-- | Top-level parser for policies, using 'Text' as input
policyParser :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
policyParser = spaceConsumer *> pSurfacePolicy <* eof

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

-- | Parses an integer literal.
integer :: Parser Int
integer = lexeme L.decimal

-- | Parses a variable name (Text identifier).
pVar :: Parser Text
pVar = lexeme ((pack .) . (:) <$> letterChar <*> many alphaNumChar) <?> "variable"

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
pTestTerm :: (Default t, Tag t) => Parser (BooleanTest t)
pTestTerm =
        (BTTrue <$ reserved "true")
    <|> (BTFalse <$ reserved "false")
    <|> parens pTestGuard
    <|> BTAtomic <$> pBellPairSep (~) "/~" <*> pure (rangeNotGreater 0)
    <?> "test term"

-- | Operator precedence table for 'BoundedTest'.
testOperatorTable :: (Default t, Tag t) => [[Operator Parser (BooleanTest t)]]
testOperatorTable =
    [ [ InfixL (symbol "||" $> (||*)) ] -- Logical OR (left-associative)
    ]

-- | Parses a 'BoundedTest' guard using an operator precedence table.
pTestGuard :: (Default t, Tag t) => Parser (BooleanTest t)
pTestGuard = makeExprParser pTestTerm testOperatorTable

-- | Parses an atomic 'Action' and wraps it into a 'SurfacePolicy' term.
pAtomicActionTerm :: Parser (SurfacePolicy t Action)
pAtomicActionTerm =
    Atomic <$> (
            (reserved "swap" >> parens (Swap <$> pLocation <* symbol "@" <*> pBellPair))
        <|> (reserved "trans" >> parens (Transmit <$> pLocation <* symbol "->" <*> pBellPair))
        <|> (reserved "distill" >> parens (Distill <$> pBellPair))
        <|> (reserved "create" >> parens (Create <$> pLocation))
        <|> (reserved "destroy" >> parens (Destroy <$> pBellPair))
        <|> (reserved "gen" >> parens (UnstableCreate <$> pBellPair))
    )
    <?> "atomic action"

-- * Policy parsers

-- | Parses a basic policy term, which can be an atomic action, 'one', a parenthesized policy,
-- an if-then-else statement, a while loop, bounded repetition, finite loop, let binding,
-- or a policy variable.
pPolicyTerm :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
pPolicyTerm =
        parens pSurfacePolicy
    <|> pAtomicActionTerm
    <|> (Recurse OGPOne <$ reserved "one")
    <|> (PolicyVariable <$> pVar)
    <?> "policy term"

-- | Parses a basic policy
pSurfacePolicy :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
pSurfacePolicy =
        pLet
    <|> pRepeat
    <|> pWhileN
    <|> Recurse <$> pIfThenElse
    <|> Recurse <$> pWhile
    <|> Recurse <$> pOrderedGuardedPolicyExpr
    <?> "policy"

-- | Parses a let binding: 'let x = p1 in p2'.
pLet :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
pLet =
    Let
        <$> (reserved "let" *> pVar <* symbol "=")
        <*> (pSurfacePolicy <* reserved "in")
        <*> pSurfacePolicy

-- | Parses a bounded repetition: 'n * p'.
pRepeat :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
pRepeat = Repeat <$> (reserved "repeat" *> integer) <*> pSurfacePolicy

-- | Parses a finite 'whileN' loop: 'whileN n test do p'.
pWhileN :: (Default t, Tag t) => Parser (SurfacePolicy t Action)
pWhileN =
    WhileN
        <$> (reserved "whileN" *> integer)
        <*> (spaceConsumer *> pTestGuard <* reserved "do")
        <*> pSurfacePolicy

-- | Parses an if-then-else policy: 'if <tag> then <policy1> else <policy2>'.
pIfThenElse :: (Default t, Tag t)
            => Parser (OrderedGuardedPolicy (BooleanTest t) (SurfacePolicy t Action))
pIfThenElse =
    OGPIfThenElse
        <$> (reserved "if" *> pTestGuard <* reserved "then")
        <*> pOrderedGuardedPolicy <* reserved "else"
        <*> pOrderedGuardedPolicy

-- | Parses a while loop policy: 'while <tag> do <policy>'.
pWhile :: (Default t, Tag t)
       => Parser (OrderedGuardedPolicy (BooleanTest t) (SurfacePolicy t Action))
pWhile =
    OGPWhile
        <$> (reserved "while" *> pTestGuard <* reserved "do")
        <*> pOrderedGuardedPolicy

-- | Parses a 'SurfacePolicy' using an operator precedence table.
pOrderedGuardedPolicyExpr :: (Default t, Tag t)
                          => Parser (OrderedGuardedPolicy (BooleanTest t) (SurfacePolicy t Action))
pOrderedGuardedPolicyExpr = makeExprParser (OGPAtomic <$> pPolicyTerm) operatorTable
--
-- | Parses a 'OrderedGuardedPolicy' from 'SurfacePolicy'
pOrderedGuardedPolicy
    :: (Default t, Tag t)
    => Parser (OrderedGuardedPolicy (BooleanTest a) (SurfacePolicy t Action))
pOrderedGuardedPolicy = OGPAtomic <$> pSurfacePolicy

-- | Operator precedence table for 'SurfacePolicy'.
-- The order of lists in the table defines precedence (lower index = lower precedence).
-- Operators within a list have the same precedence.
operatorTable :: (Default t, Tag t)
              => [[Operator Parser (OrderedGuardedPolicy (BooleanTest t) (SurfacePolicy t Action))]]
operatorTable =
    [ [ InfixN (symbol "<.>" $> OGPOrdered)   ] -- Ordered composition (non-associative)
    , [ InfixL (symbol "<||>" $> OGPParallel) ] -- Parallel composition (left-associative)
    , [ InfixL (symbol "<>" $> OGPSequence)   ] -- Sequential composition (left-associative)
    ]
