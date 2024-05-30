module Metamorth.Lift.Pat
  ( liftPat ) where

import Control.Monad ((>=>))

import Data.Maybe

import Language.Haskell.TH.Syntax

liftPat :: (Lift a) => a -> Q (Maybe Pat)
liftPat x = do
  y <- lift x
  return $ liftPat' y

liftPat' :: Exp -> Maybe Pat
liftPat' y =
  case y of
    ConE nom -> Just $ ConP nom [] []
    LitE val -> Just $ LitP val
    VarE nom -> Just $ VarP nom -- kinda meaningless here.

    AppE expr1 expr2 -> liftPatApp expr1 expr2

    (AppTypeE expr typ) -> (\rslt -> SigP rslt typ) <$> liftPat' expr
    ParensE expr -> ParensP <$> liftPat' expr
    ListE exps -> ListP <$> mapAll liftPat' exps
    TupE exps  -> TupP <$> mapAll (id >=> liftPat') exps
    UnboxedTupE exps  -> UnboxedTupP <$> mapAll (id >=> liftPat') exps

    RecConE nom exps -> RecP nom <$> mapAll (onSecond liftPat') exps

    -- Unsure what this is for; both Exp and Pat have
    -- similar constructors.
    UnboxedSumE expr salt sar -> UnboxedSumP <$> liftPat' expr <*> pure salt <*> pure sar
    
    -- Unsure whether this can be broadened.
    (InfixE (Just expr1) (ConE nom) (Just expr3)) -> InfixP <$> liftPat' expr1 <*> pure nom <*> liftPat' expr3
    InfixE {} -> Nothing

    (UInfixE expr1 (ConE nom) expr3) -> UInfixP <$> liftPat' expr1 <*> pure nom <*> liftPat' expr3
    UInfixE {} -> Nothing

    UnboundVarE _ -> Just WildP -- maybe change this

    SigE expr typ -> SigP <$> liftPat' expr <*> pure typ

    -- Might be able to be non-failures?
    CompE {} -> Nothing
    ArithSeqE {} -> Nothing
    StaticE {} -> Nothing
    LabelE {} -> Nothing
    GetFieldE _ _ -> Nothing
    ProjectionE {} -> Nothing
    ImplicitParamVarE _ -> Nothing
    -- TypeE {} -> Nothing


    -- Auto-failures:
    LamE _ _ -> Nothing
    LamCaseE _ -> Nothing
    LamCasesE _ -> Nothing
    CondE {} -> Nothing
    MultiIfE _ -> Nothing
    RecUpdE _ _ -> Nothing

    LetE {} -> Nothing
    CaseE {} -> Nothing

    DoE  {} -> Nothing
    MDoE {} -> Nothing

data AppResult = AppResult
  { appConstructor :: Name
  , appArgs        :: [Pat]
  }

addArg :: Pat -> AppResult -> AppResult
addArg pat (AppResult nom pats) = AppResult nom (pat:pats)

liftPatApp :: Exp -> Exp -> Maybe Pat
liftPatApp expr1 expr2 = case liftPatApp' expr1 expr2 of
  Nothing -> Nothing
  Just (AppResult nom args) -> Just (ConP nom [] (reverse args))

liftPatApp' :: Exp -> Exp -> Maybe AppResult
liftPatApp' (ConE nom) expr = AppResult nom <$> ((:[]) <$> liftPat' expr)
liftPatApp' (AppE expr1 expr2) expr3 = do
  rslt3 <- liftPat' expr3
  rslt1 <- liftPatApp' expr1 expr2
  return $ addArg rslt3 rslt1
liftPatApp' _ _ = Nothing -- Unsure whether we need to cover other cases.




-- | Ensure that every result of a computation
--   is a `Just`. Return `Nothing` if any value
--   is `Nothing`.
mapAll :: (a -> Maybe b) -> [a] -> Maybe [b]
mapAll f xs = case (map f xs) of
  ys | any isNothing ys -> Nothing
     | otherwise        -> Just $ catMaybes ys

onSecond :: (b -> Maybe c) -> (a, b) -> Maybe (a, c)
onSecond f (x,y) = case (f y) of
  Nothing  -> Nothing
  (Just z) -> Just (x,z)


{-
data MyExample = MyExample Bool Int String deriving (Show, Lift, Eq)
lift (MyExample True 9 "okay")
AppE 
  (AppE 
    (AppE 
      (ConE Ghci5.MyExample) 
      (ConE GHC.Types.True)
    ) 
    (LitE (IntegerL 9))) (ListE [LitE (CharL 'o'),LitE (CharL 'k'),LitE (CharL 'a'),LitE (CharL 'y')])
-}


