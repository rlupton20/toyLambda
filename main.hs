-- Lets build a parser for the lambda calculus
-- or at least something like it :P

import Text.Parsec as Parsec hiding ((<|>))
import Control.Applicative

data Term a = Atom a | Abstraction (Term a) (Term a) | Application (Term a) (Term a) deriving (Show, Eq)
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

-- The following parses lambda terms. It identifies either an atom,
-- or it strips left and right parentheses and then checks inside for
-- either a lambda abstraction, or an application. This should be faster
-- than using try excessively, with a "more natural" definition of
-- application and abstraction below.
term :: Parsec String () LambdaTerm
term = try(atom) <|> (lpar *> (application <|> abstraction) <* rpar)

-- atoms here must start with a letter
atom :: Parsec String () LambdaTerm
atom = do
	h <- letter
	rest <- Parsec.many alphaNum
	return (Atom (h:rest))

-- Remember we stripped the parenthesese
abstraction :: Parsec String () LambdaTerm
abstraction = Abstraction <$> (lambda *> atom) <*> (dot *> term)

-- Remember we stripped the parenthesese
application :: Parsec String () LambdaTerm
application = Application <$> (term <* apply) <*> term

-- Test function, example "test term "lambda_expression""
-- Note lambda must be escaped, and written \\	
test :: Parsec String () a -> String -> Either ParseError a
test par str = parse par ("Input string: " ++ str) str

lambdaTerm :: String -> Either ParseError LambdaTerm
lambdaTerm = test term

-- Best used in applicative style with lambdaTerm
substitute :: (Eq a) => Term a -> Term a -> Term a -> Term a
substitute (Atom var) term (Atom var') = if var' == var then term else (Atom var')
substitute atom@(Atom var) term (Application t1 t2) = (Application (substitute atom term t1) (substitute atom term t2))
substitute atom term abstr@(Abstraction var' t) = if atom == var' then abstr else Abstraction var' $ substitute atom term t
substitute _ _ term = term

-- Again, use in an applicative style
beta :: (Eq a) => Term a -> Term a
beta (Atom x)  = Atom x
beta term@(Application f s) = case f of (Abstraction x t) -> beta $ substitute x s t
					(Application g r) -> beta $ (Application (beta $ Application g r) s)
					_ -> term
beta term = term

-- Trying to reduce an irreducible term -- break out with Ctrl + C
example1 = beta <$> lambdaTerm "((\\x.(x|x))|(\\x.(x|x)))"
-- Currying
example2 = beta <$> lambdaTerm "(((\\x.(\\y.(x|y)))|a)|b)"
