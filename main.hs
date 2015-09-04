-- Lets build a parser for the lambda calculus
-- or at least something like it :P

import Text.Parsec as Parsec hiding ((<|>))
import Control.Applicative

data Term a = Atom a | Abstraction (Term a) (Term a) | Application (Term a) (Term a) deriving Show
type LambdaTerm = Term String

wrapSpaces :: Parsec String () a -> Parsec String () a
wrapSpaces parser = spaces *> parser <* spaces

lpar :: Parsec String () Char
lpar = wrapSpaces $ char '('

rpar :: Parsec String () Char
rpar =  wrapSpaces $ char ')'

dot :: Parsec String () Char
dot = wrapSpaces $ char '.'

lambda :: Parsec String () Char
lambda = wrapSpaces $ char '\\'

apply :: Parsec String () Char
apply = wrapSpaces $ char '|'

atom :: Parsec String () LambdaTerm
atom = do
	h <- letter
	rest <- Parsec.many alphaNum
	return (Atom (h:rest))

abstraction :: Parsec String () LambdaTerm
abstraction = Abstraction <$> (lpar *> lambda *> atom) <*> (dot *> term <* rpar)

term :: Parsec String () LambdaTerm
term = try(atom) <|> try(application) <|> abstraction

application :: Parsec String () LambdaTerm
application = Application <$> (lpar *> term <* apply) <*> (term <* rpar)
	
test :: Parsec String () a -> String -> Either ParseError a
test par str = parse par ("Input string: " ++ str) str
