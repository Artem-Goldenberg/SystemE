module Actions (module Actions) where

import Data.Maybe
import Data.Function

import Format

infixl 8 /\  -- join operator used in the other module
infixl 9 @   -- generalized expansion var app

class Join a where
    (/\) :: a -> a -> a

instance Join Type where
    (/\) :: Type -> Type -> Type
    I as /\ I bs = I $ as ++ bs
    I as /\ b = I $ as ++ [b]
    a /\ I bs = I $ a : bs
    a /\ b = I [a, b]

instance Join Context where
    (/\) :: Context -> Context -> Context
    c1 /\ c2 = meet c1 c2
      where
        meet acc ((x, a) : ctx) = case lookup x acc of
            Just b -> meet (insert acc x $ a /\ b) ctx
            Nothing -> meet ((x, a) : acc) ctx
        meet acc [] = acc

instance Join Expansion where
    (/\) :: Expansion -> Expansion -> Expansion
    E as /\ E bs = E $ as ++ bs
    E as /\ b = E $ as ++ [b]
    a /\ E bs = E $ a : bs
    a /\ b = E [a, b]


class AddEVar a where
    (@) :: EVar -> a -> a

instance AddEVar Type where
    (@) :: EVar -> Type -> Type
    e @ a = e :@ a

instance AddEVar Context where
    (@) :: EVar -> Context -> Context
    e @ ctx = map ((e @) <$>) ctx

instance AddEVar [Constraint] where
    (@) :: EVar -> [Constraint] -> [Constraint]
    e @ eqs = (\(a :=: b) -> e @ a :=: e @ b) <$> eqs

instance AddEVar Expansion where
    (@) :: EVar -> Expansion -> Expansion
    e @ exp = e :* exp


class Substitute a where
    type SubstitutionResult a
    type SubstitutionResult a = a
    substitute :: Substitution -> a -> SubstitutionResult a

instance Substitute TVar where
    type SubstitutionResult TVar = Type
    substitute :: Substitution -> TVar -> Type
    substitute s alpha = fromMaybe (TVar alpha) $ lookupType s alpha

instance Substitute EVar where
    type SubstitutionResult EVar = Expansion
    substitute :: Substitution -> EVar -> Expansion
    substitute s e = fromMaybe (e :* Substitution []) $ lookupExp s e

instance Substitute Type where
    substitute :: Substitution -> Type -> Type
    substitute s = \case
        TVar alpha -> substitute s alpha
        e :@ a -> substitute s e `expand` a
        a :-> b -> ((:->) `on` substitute s) a b
        I as -> I $ substitute s <$> as

instance Substitute Context where
    substitute :: Substitution -> Context -> Context
    substitute s ctx = [(t, substitute s a) | (t, a) <- ctx]

instance Substitute Constraint where
    substitute :: Substitution -> Constraint -> Constraint
    substitute s (a :=: b) = ((:=:) `on` substitute s) a b

instance Substitute Typing where
    substitute :: Substitution -> Typing -> Typing
    substitute s (ctx :- a) = substitute s ctx :- substitute s a

instance Substitute Substitution where
    substitute :: Substitution -> Substitution -> Substitution
    substitute s [] = s
    substitute s (alpha := a : s') = alpha := substitute s a : substitute s s'
    substitute s (e :== exp : s') = e :== substitute s exp : substitute s s'

instance Substitute Expansion where
    substitute :: Substitution -> Expansion -> Expansion
    substitute s = \case
        Substitution s' -> Substitution $ substitute s s'
        e :* exp -> substitute s e `expand` exp
        E exps -> E $ map (substitute s) exps


class Substitute a => Expand a where
    expand :: Expansion -> a -> a

instance Expand Type where
    expand :: Expansion -> Type -> Type
    expand exp a = case exp of
        Substitution s -> substitute s a
        e :* exp -> e :@ expand exp a
        E exps -> foldr ((/\) . flip expand a) om exps

instance Expand Context where
    expand :: Expansion -> Context -> Context
    expand exp ctx = [(t, expand exp a) | (t, a) <- ctx]

instance Expand Expansion where
    expand :: Expansion -> Expansion -> Expansion
    expand exp exp' = case exp of
        Substitution s -> substitute s exp'
        e :* exp -> e :* expand exp exp'
        E exps -> foldr ((/\) . flip expand exp') (E []) exps
