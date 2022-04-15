module LangLib where

import qualified Data.Map as Map
import AST
import Prelude hiding (and, not, or, succ, pred, head, tail, null)

{-- Library Functions --}

true = InLam ["x", "y"] (InVar "x")
false = InLam ["x", "y"] (InVar "y")
ifthenelse = InLam ["p", "a", "b"] (InApp [InVar "p", InVar "a", InVar "b"])

not = InLam ["p", "q"] (InApp [InVar "p", false, true])
isZero = InLam ["n"] (InApp [InVar "n", InLam ["x"] false, true])

zero = InLam ["f", "x"] (InVar "x")
succ = InLam ["n", "f", "x"] (InApp [InVar "f", InApp [InVar "n", InVar "f", InVar "x"]])
pred = InLam ["n", "f", "x"] (InApp [InVar "n", InLam ["g", "h"] (InApp [InVar "h", InApp [InVar "g", InVar "f"]]), InLam ["u"] (InVar "x"), InLam ["u"] (InVar "u")])

bident = InLam ["f", "x"] (InApp [InVar "f", InVar "x"])

or = InLam ["p", "q"] (InApp [InVar "p", InVar "p", InVar "q"])
and = InLam ["p", "q"] (InApp [InVar "p", InVar "q", InVar "p"])

eq = InLam ["m", "n"] (InApp [and, InApp [leq, InVar "m", InVar "n"], InApp [leq, InVar "n", InVar "m"]])
neq = InLam ["m", "n"] (InApp [not, InApp [eq, InVar "m", InVar "n"]])
lt = InLam ["m", "n"] (InApp [and, InApp [leq, InVar "m", InVar "n"], InApp [neq, InVar "m", InVar "n"]])
leq = InLam ["m", "n"] (InApp [isZero, InApp [sub, InVar "m", InVar "n"]])
gt = InLam ["m", "n"] (InApp [not, InApp [leq, InVar "m", InVar "n"]])
geq = InLam ["m", "n"] (InApp [or, InApp [gt, InVar "m", InVar "n"], InApp [eq, InVar "m", InVar "n"]])

plus = InLam ["m", "n", "f", "x"] (InApp [InVar "m", InVar "f", InApp [InVar "n", InVar "f", InVar "x"]])
sub = InLam ["m", "n"] (InApp [InVar "n", pred, InVar "m"])
mult = InLam ["m", "n", "f"] (InApp [InVar "m", InApp [InVar "n", InVar "f"]])
pow = InLam ["b", "e"] (InApp [InVar "e", InVar "b"])

pair = InLam ["x", "y", "f"] (InApp [InVar "f", InVar "x", InVar "y"])
nil = InLam ["x"] true

comp = InLam ["f", "g", "x"] (InApp [InVar "f", InApp [InVar "g", InVar "x"]])

head = InLam ["p"] (InApp [InVar "p", InLam ["x", "y"] (InVar "x")])
tail = InLam ["p"] (InApp [InVar "p", InLam ["x", "y"] (InVar "y")])
null = InLam ["p"] (InApp [InVar "p", InLam ["x", "y"] (InLam ["x", "y"] (InVar "y"))])

fix = InLam ["f"] (InApp [InLam ["x"] (InApp [InVar "f", InApp [InVar "x", InVar "x"]]),
                               InLam ["x"] (InApp [InVar "f", InApp [InVar "x", InVar "x"]])])


type Precedence = Int

data OpAss = LeftAss | RightAss
  deriving (Show)

data Operator = Op Precedence OpAss InTerm
  deriving (Show)

type Operators = Map.Map String Operator

operators :: Operators
operators = Map.fromList
 [("$",  Op 0 RightAss bident),
  ("&",  Op 1 LeftAss  bident),
  ("||", Op 2 RightAss or),
  ("&&", Op 3 RightAss and),
  ("==", Op 4 LeftAss  eq),
  ("/=", Op 4 LeftAss  neq),
  ("<",  Op 4 LeftAss  lt),
  ("<=", Op 4 LeftAss  leq),
  (">",  Op 4 LeftAss  gt),
  (">=", Op 4 LeftAss  geq),
  ("+",  Op 6 LeftAss  plus),
  ("-",  Op 6 LeftAss  sub),
  ("*",  Op 6 LeftAss  mult),
  ("/",  Op 7 LeftAss  (InLam ["f", "x"] (InApp [InVar "f", InVar "x"]))),
  ("^",  Op 8 RightAss pow),
  ("!!", Op 9 LeftAss  (InLam ["f", "x"] (InApp [InVar "f", InVar "x"]))),
  (":",  Op 9 RightAss pair),
  (".",  Op 9 RightAss comp)]

type LibraryFns = Map.Map Name InTerm

libFns :: LibraryFns
libFns = Map.fromList
  [("head", head),
   ("tail", tail),
   ("null", null),
   ("succ", succ),
   ("pred", pred),
   ("not", not),
   ("fix", fix)]

convert :: InTerm -> Term
convert (InVar name)
  | Map.member name libFns = convert $ libFns Map.! name
  | otherwise = Var name
convert (InLam names term) = foldr Lam (convert term) names
convert (InApp terms) = foldl1 App $ map convert terms
-- TODO: rearrange AST
convert (InOp op term term') = let (Op _ _ fn) = operators Map.! op in convert $ InApp [fn, term, term']
convert (InLet [] term inTerm) = error "empty let expression"
convert (InLet (name:names) term inTerm) = convert $ InApp [InLam [name] inTerm, InLam names term]
convert (InLetrec [] term inTerm) = error "empty letrec expression"
convert (InLetrec (name:names) term inTerm) = convert $ InApp [InLam [name] inTerm, InApp [fix, InLam [name] (InLam names term)]]
convert (InIfThenElse cond termIfTrue termIfFalse) = convert $ InApp [ifthenelse, cond, termIfTrue, termIfFalse]
convert (InLit lit) =
  case lit of
    (LitNat n) -> convert $ natToChurch n
    (LitBool True) -> convert true
    (LitBool False) -> convert false
    (LitList terms) -> convert $ foldr (InOp ":") nil terms
convert (InParen term) = convert term

natToChurch :: Int -> InTerm
natToChurch 0 = zero
natToChurch n = InApp [succ, natToChurch $ n - 1]
