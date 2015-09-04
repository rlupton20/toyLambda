-- Lets build a parser for the lambda calculus
-- or at least something like it :P

import Text.Parsec as Parsec hiding ((<|>))
import Control.Applicative

data Term a = Atom a | Abstraction (Term a) (Term a) | Application (Term a) (Term a) deriving (Eq, Show)
type LambdaTerm = Term String

main :: IO ()
main = do
	putStr ">"
	str <- getLine
	putStrLn.format $ beta <$> lambdaTerm str
	main

-- Lets write a nice formatter for lambda terms
-- (later this has to be used applicatively
format :: Either ParseError LambdaTerm -> String
format (Right (Atom x)) = x
format (Right (Abstraction x t)) = "(\\" ++ format (Right x) ++ "." ++ format (Right t) ++ ")"
format (Right (Application s t)) = format (Right s) ++ format (Right t) 
format (Left err) = show err

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

-- This parses a string into a lambda term
lambdaTerm :: String -> Either ParseError LambdaTerm
lambdaTerm str = parse term ("Input string: " ++ str) str

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
