-- Lets build a parser for the lambda calculus
-- or at least something like it :P

import System.IO
import Text.Parsec as Parsec hiding ((<|>))
import Control.Applicative

data Term a = Atom a | Abstraction (Term a) (Term a) | Application (Term a) (Term a) deriving (Eq, Show)
type LambdaTerm = Term String

main :: IO ()
main = do
	putStr "\955ambda> "
	hFlush stdout	-- Apparently stdout is line buffered, so without this, no prompt!
	str <- getLine
	putStrLn.format $ eval <$> lambdaTerm str
	main

-- Lets write a nice formatter for lambda terms
-- Here lets deal with the fact everything is wrapped
-- in the (Either ParseError) monad.
format :: Either ParseError LambdaTerm -> String
format (Right (Atom x)) = x
format (Right (Abstraction x t)) = "(\\" ++ format (Right x) ++ "." ++ format (Right t) ++ ")"
format (Right (Application s t)) = "(" ++ format (Right s) ++ format (Right t) ++ ")"
format (Left err) = show err

wrapSpaces :: Parsec String () a -> Parsec String () a
wrapSpaces parser = spaces *> parser <* spaces

wrapChar :: Char -> Parsec String () Char
wrapChar = wrapSpaces.char

-- The following parses lambda terms. It identifies either an atom,
-- or it strips left and right parentheses and then checks inside for
-- either a lambda abstraction, or an application. This should be faster
-- than using try excessively, with a "more natural" definition of
-- application and abstraction below.
term :: Parsec String () LambdaTerm
term = try(atom) <|> (wrapChar '(' *> (application <|> abstraction) <* wrapChar ')')

-- atoms here must start with a letter
atom :: Parsec String () LambdaTerm
atom = do
	h <- letter
	rest <- Parsec.many alphaNum
	return (Atom (h:rest))

-- Remember we stripped the parenthesese
abstraction :: Parsec String () LambdaTerm
abstraction = Abstraction <$> (lambda *> atom) <*> (dot *> term)
	where 	lambda = wrapChar '\\'
		dot = wrapChar '.'

-- Remember we stripped the parenthesese
application :: Parsec String () LambdaTerm
application = Application <$> (term <* apply) <*> term
	where apply = wrapChar '|'

-- This parses a string into a lambda term
lambdaTerm :: String -> Either ParseError LambdaTerm
lambdaTerm str = parse term ("Input string: " ++ str) str

-- Best used in applicative style with lambdaTerm
substitute :: (Eq a) => Term a -> Term a -> Term a -> Term a
substitute (Atom v) term (Atom v') = if v' == v then term else (Atom v')
substitute atom@(Atom v) term (Application t1 t2) = (Application (substitute atom term t1) (substitute atom term t2))
substitute atom@(Atom _) term abstr@(Abstraction v' t) = if atom == v' then abstr else Abstraction v' $ substitute atom term t
substitute _ _ term = term

-- Again, use in an applicative style
-- beta applies an abstraction to a term
beta :: (Eq a) => Term a -> Term a
beta (Atom x)  = Atom x
beta term@(Application f s) = case f of (Abstraction x t) -> substitute x s t
                                        _ -> term
beta term = term

-- eval rebuild lambda expressions from the ground
-- but beta reduces when it hits an abstraction applied
-- to a term. Since this may introduce an abstraction term
-- where only an atom was before, we need to start eval over
-- on the beta reduced term.
eval :: (Eq a) => Term a -> Term a
eval (Atom x) = Atom x
eval (Application t1 t2) = case (eval t1) of
  (Abstraction _ _) -> eval.beta $ Application (eval t1) (eval t2)
  _ -> Application (eval t1) (eval t2)
eval (Abstraction x t) = Abstraction x (eval t)
