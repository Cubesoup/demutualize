{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Language.LBNF
  
-- Note: "mu" is present so that I don't need to have two types representing types hanging around.
-- The idea that that you write recursive types via recursive delcaration, as in
-- list = all a:* . 1 + (a * (list a))
-- and the result of "demutualizing" the signature is declarations using mu, as in
-- list = mu list . all a:* . 1 + (a * (list a))

-- Also Note: There are 98 (!!!) shift/reduce conflicts. If you want it to parse your
-- type correctly, use tons of parentheses. Planning to add operator precedence next.

bnfc [lbnf|

TMu.   Ty ::= "mu" Ident "." Ty;
TUnit. Ty ::= "1";
TProd. Ty ::= Ty "*" Ty;
TSum.  Ty ::= Ty "+" Ty;
TVar.  Ty ::= Ident;
TAll.  Ty ::= "all" Ident ":" Kind "." Ty;
TAbs.  Ty ::= "lam" Ident ":" Kind "." Ty;
TApp.  Ty ::= Ty Ty;
_.     Ty ::= "(" Ty ")";

KStar. Kind ::= "*";
KTyOp. Kind ::= Kind "->" Kind;

TyDecl. Decl ::= Ident " = " Ty;

entrypoints Ty, Kind, Decl;

  |]

-- generates the following types:

--  data Ty
--  = TUnit
--  | TProd Ty Ty
--  | TSum Ty Ty
--  | TVar Ident
--  | TAll Ident Kind Ty
--  | TAbs Ident Kind Ty
--  | TApp Ty Ty
--  | TMu  Ident Ty

-- data Kind
-- = KStar | KTyOp Kind Kind

-- data ParseMonad a = Ok a | Bad String
  
-- and functions:

-- myLexer :: String -> [Token]
-- pTy :: [Token] -> ParseMonad Ty
-- pKind :: [Token] -> ParseMonad Kind
-- pDecl :: [Toke] -> ParseMonad Decl


-- I've hidden the pretty-printers at the bottom here!  

prettyTy :: Ty -> String
prettyTy TUnit = "1"
prettyTy (TVar (Ident x)) = x
prettyTy (TProd t1 t2) = (prettyTy t1) ++ " * " ++ (prettyTy t2)
prettyTy (TSum  t1 t2) = (prettyTy t1) ++ " + " ++ (prettyTy t2)
prettyTy (TApp  t1 t2) = (prettyTy t1) ++ " " ++ (prettyTy t2)
prettyTy (TAll (Ident x) k t) = "(all " ++ x ++ ":" ++ (prettyKind k) ++ " . " ++ (prettyTy t) ++ ")"
prettyTy (TAbs (Ident x) k t) = "(lam " ++ x ++ ":" ++ (prettyKind k) ++ " . " ++ (prettyTy t) ++")"
prettyTy (TMu  (Ident x) t) = "(mu "  ++ x ++ " . " ++ (prettyTy t) ++ ")"

prettyKind :: Kind -> String
prettyKind KStar = "*"
prettyKind (KTyOp k1 k2) = (prettyKind k1) ++ "->" ++ (prettyKind k2)

prettyDecl :: Decl -> String
prettyDecl (TyDecl (Ident x) t) = x ++ " = " ++ (prettyTy t)
