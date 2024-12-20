module Algorithms (module Algorithms) where

import Data.List
import Data.Function
import Control.Monad.State
import GHC.Stack (HasCallStack)
import Format
import Actions

-- infix 9 ~

-- (~) :: Type -> Type -> Substitution
-- a ~ b = undefined

-- unify :: HasCallStack => Constraint -> Substitution
-- unify = \case
--     TVar alpha :=: b -> [alpha := b]
--     a :=: TVar beta -> [beta := a]
--     a :-> b :=: a' :-> b' -> (unify `on` ((/\) . (:=:))) a a' b b'
--     _ -> undefined

refreshWith :: Int -> Type -> Substitution
refreshWith fresh = \case
    TVar alpha@(TypeVar name) -> [alpha := TVar alpha']
        where alpha' = TypeVar $ name ++ show fresh
    e@(ExpVar name) :@ a -> e :== e' :* Substitution [] : refreshWith fresh a
        where e' = ExpVar $ name ++ show fresh
    a :-> b -> ((++) `on` refreshWith fresh) a b
    I as -> concatMap (refreshWith fresh) as

-- computeRefresh :: State (Int, Type) Substitution
-- computeRefresh = do
--     (fresh, a) <- get 
--     case a of 
--         TVar alpha -> 

infer :: Term -> Typing
infer t = let 
    (eqs, tp) = initial t
    s = solve eqs
    in substitute s tp

solve :: [Constraint] -> Substitution
solve eqs = concat $ helper [] eqs
    where
        helper acc [] = acc
        helper acc (eq:eqs) 
            | (Just s) <- unifyBeta eq = helper (s:acc) $ substitute s <$> eqs
            | otherwise = let s = simpleUnify eq in helper (s:acc) $ substitute s <$> eqs

unifyApp :: HasCallStack => Constraint -> Substitution
unifyApp (TVar alpha :=: b@(_ :@ _ :-> TVar _)) = [alpha := b]
unifyApp eq = error $ "Cannot handle constraint: " ++ show eq

unifyBeta :: Constraint -> Maybe Substitution
unifyBeta (a :-> b :=: e :@ c :-> TVar alpha) = let
    s1 = [e :== evalState computeShapeExpansion (0, a)]
    s2 = simpleUnify $ a :=: substitute s1 (e :@ c)
    s3 = [alpha := substitute s2 b]
    in Just $ s1 ++ s2 ++ s3
unifyBeta _ = Nothing

simpleUnify :: HasCallStack => Constraint -> Substitution
simpleUnify = \case
    TVar alpha :=: b -> [alpha := b]
    a :=: TVar beta -> [beta := a]
    e :@ a :=: e' :@ b | e == e' -> simpleUnify $ a :=: b
    a :-> b :=: a' :-> b' -> simpleUnify (a :=: a') ++ simpleUnify (b :=: b')
    I as :=: I bs | length as == length bs -> concatMap simpleUnify $ zipWith (:=:) as bs
    eq -> error $ "Cannot handle constraint: " ++ show eq

computeShapeExpansion :: State (Int, Type) Expansion
computeShapeExpansion = do
    (fresh, a) <- get
    case a of
        e :@ a -> do
            put (fresh, a)
            exp <- computeShapeExpansion
            return $ e :* exp
        I as -> E <$> mapM (\a -> changeType a >> computeShapeExpansion) as
            where changeType a = modify (const a <$>)
        _ -> Substitution <$> do
            put (fresh + 1, a)
            return $ refreshWith fresh a

-- extractShape :: Type -> Expansion
-- extractShape = \case


-- unifyBeta eq = error $ "Don't know how to deal with: " ++ show eq

epath :: Constraint -> [EVar]
epath ((e :@ a) :=: (e' :@ b)) | e == e' = e : epath (a :=: b)
epath _ = []

tvar :: Int -> Type
-- tvar fresh = TVar . TypeVar $ "a" ++ show fresh
tvar = (!!) $ TVar . TypeVar . singleton <$> filter (/= 'e') ['a'..]

evar :: Int -> EVar
evar fresh = ExpVar $ "e" ++ show fresh

initial :: Term -> ([Constraint], Typing)
initial t = evalState computeInitial (0, t)

computeInitial :: State (Int, Term) ([Constraint], Typing)
computeInitial = do
    (fresh, t) <- get
    case t of
        Var x -> do
            put (fresh + 1, t)
            return ([], [(x, tvar fresh)] :- tvar fresh)
        Lam x t -> do
            put (fresh, t)
            (eqs, ctx :- a) <- computeInitial
            let b = lookupContext ctx x
            return (eqs, dropInContext ctx x :- b :-> a)
        App t t' -> do
            put (fresh, t)
            (eqs1, ctx1 :- a) <- computeInitial
            modify (const t' <$>) -- replace term in state with t'
            (eqs2, ctx2 :- b) <- computeInitial
            (fresh, t) <- get
            put (fresh + 1, t)
            let alpha = tvar fresh; e = evar fresh
            return (a :=: e :@ b :-> alpha : (eqs1 ++ e @ eqs2), ctx1 /\ e @ ctx2 :- alpha)
