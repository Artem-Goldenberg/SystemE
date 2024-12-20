module Format (module Format) where

import Data.List
import Data.Maybe

{- Some conventions:
    Terms: t
    Term Variables: x y z
    Type Variables: alpha, beta?
    Types: a b c ...
    Context: ctx
    Substitutions: s
    Expansion: exp, epx'?
    Expansion variables: e, e'
    Constraint: eq
    Many Constraints: eqs
    Typing: tp
-}

-- All Operators Here
infixr 7 :->  -- arrow type
infix  4 :-   -- in context
infixl 9 :@   -- expansion var app in type
infixl 9 :*   -- expansion var app in expansion
infixl 6 :=   -- type substitution
infixl 6 :==  -- expansion substitution
infix  6 :=:  -- constraint on types

-- infixl 8 /\   -- join operator used in the other module
-- infixl 9 @    -- generalized expansion var app

data family Var a

newtype instance Var Term = TermVar String
    deriving (Eq, Show)
newtype instance Var Type = TypeVar String
    deriving (Eq, Show)
newtype instance Var Expansion = ExpVar String
    deriving (Eq, Show)

type TVar = Var Type
type EVar = Var Expansion

data Term = Var (Var Term) | App Term Term | Lam (Var Term) Term deriving (Eq, Show)

data Type = TVar TVar | EVar :@ Type | Type :-> Type | I [Type] deriving Eq

data Assignment = TVar := Type | EVar :== Expansion
    deriving (Eq, Show)

type Substitution = [Assignment]

data Expansion
    = Substitution Substitution
    | EVar :* Expansion
    | E [Expansion]
    deriving (Eq, Show)

type Context = [(Var Term, Type)]

data Typing = Context :- Type deriving (Eq, Show)
data Constraint = Type :=: Type deriving (Eq, Show)

lookupExp :: Substitution -> EVar -> Maybe Expansion
lookupExp s e = listToMaybe [exp | e' :== exp <- s, e == e']

lookupType :: Substitution -> TVar -> Maybe Type
lookupType s alpha = listToMaybe [b | beta := b <- s, alpha == beta]

lookupContext :: Context -> Var Term -> Type
lookupContext ctx x = maybe om snd (find ((== x) . fst) ctx)

dropInContext :: Context -> Var Term -> Context
dropInContext ctx x = filter ((/= x) . fst) ctx

insert :: Context -> Var Term -> Type -> Context
insert ctx x a = fromMaybe ((x, a) : ctx) $ do
    i <- findIndex ((== x) . fst) ctx
    let (ctx1, _ : ctx2) = splitAt i ctx
    return $ ctx1 ++ (x, a) : ctx2

om :: Type
om = I []

instance Show Type where
    show :: Type -> String
    show = \case
        TVar (TypeVar alpha) -> alpha
        (ExpVar e) :@ a -> e ++ " " ++ show a
        a :-> b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
        I [] -> "ω"
        I as -> intercalate  " /\\ " $ show <$> as
