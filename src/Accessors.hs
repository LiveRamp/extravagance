{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Accessors where

import           Language.Java.Syntax

identString :: Ident -> String
identString (Ident s) = s

class Modified m where
    getModifiers :: m -> [Modifier]
    setModifiers :: [Modifier] -> m -> m
    mapModifiers :: ([Modifier] -> [Modifier]) -> m -> m
    mapModifiers f m = setModifiers (f (getModifiers m)) m

instance Modified MemberDecl where
    getModifiers (MethodDecl ms _ _ _ _ _ _ _)  = ms
    getModifiers (FieldDecl ms _ _)             = ms
    getModifiers (ConstructorDecl ms _ _ _ _ _) = ms
    getModifiers decl                           = []

    setModifiers ms (MethodDecl _ a b c d e f g)  = MethodDecl ms a b c d e f g
    setModifiers ms (FieldDecl _ a b)             = FieldDecl ms a b
    setModifiers ms (ConstructorDecl _ a b c d e) = ConstructorDecl ms a b c d e
    setModifiers ms decl                          = decl

instance Modified Decl where
    getModifiers (MemberDecl m) = getModifiers m
    getModifiers _              = []

    setModifiers ms (MemberDecl m) = MemberDecl $ setModifiers ms m
    setModifiers ms decl           = decl

class Identified i where
    getIdentifier :: i -> Ident
    setIdentifier :: Ident -> i -> i
    mapIdentifier :: (Ident -> Ident) -> i -> i
    getIdentString :: i -> String
    setIdentString :: String -> i -> i
    mapIdentString :: (String -> String) -> i -> i
    mapIdentifier f i = setIdentifier (f (getIdentifier i)) i
    getIdentString = identString . getIdentifier
    setIdentString s = setIdentifier (Ident s)
    mapIdentString f i = setIdentString (f (getIdentString i)) i

instance Identified VarDeclId where
    getIdentifier (VarId i)        = i
    getIdentifier (VarDeclArray v) = getIdentifier v
    setIdentifier i (VarId _)        = VarId i
    setIdentifier i (VarDeclArray v) = setIdentifier i v

instance Identified VarDecl where
    getIdentifier (VarDecl id _) = getIdentifier id
    setIdentifier i (VarDecl id a) = VarDecl (setIdentifier i id) a


instance Identified MemberDecl where
    getIdentifier (MethodDecl _ _ _ id _ _ _ _)  = id
    getIdentifier (FieldDecl _ _ (firstVar : otherVars))  = getIdentifier firstVar
    getIdentifier (ConstructorDecl _ _ id _ _ _) = id
    getIdentifier (MemberClassDecl (ClassDecl _ id _ _ _ _ )) = id
    getIdentifier (MemberInterfaceDecl (InterfaceDecl _ _ id _ _ _ )) = id
    getIdentifier _ = Ident ""

    setIdentifier id (MethodDecl a b c _ d e f g)  = MethodDecl a b c id d e f g
    setIdentifier id (FieldDecl a b (firstVar : otherVars))  = FieldDecl a b (setIdentifier id firstVar : otherVars)
    setIdentifier id (ConstructorDecl a b _ c d e) = ConstructorDecl a b id c d e
    setIdentifier id (MemberClassDecl (ClassDecl a _ b c d e )) = MemberClassDecl (ClassDecl a id b c d e )
    setIdentifier id (MemberInterfaceDecl (InterfaceDecl a b _ c d e )) = MemberInterfaceDecl (InterfaceDecl a b id c d e )
    setIdentifier id m = m

instance Identified ClassDecl where
    getIdentifier (ClassDecl _ id _ _ _ _) = id
    getIdentifier (EnumDecl _ id _ _)      = id
    setIdentifier id (ClassDecl a _ b c d e) = ClassDecl a id b c d e
    setIdentifier id (EnumDecl a _ b c)      = EnumDecl a id b c

instance Identified InterfaceDecl where
    getIdentifier (InterfaceDecl _ _ id _ _ _) = id
    setIdentifier id (InterfaceDecl a b _ c d e) = InterfaceDecl a b id c d e

instance Identified TypeDecl where
    getIdentifier (ClassTypeDecl classDecl) = getIdentifier classDecl
    getIdentifier (InterfaceTypeDecl interfaceDecl) = getIdentifier interfaceDecl
    setIdentifier id (ClassTypeDecl classDecl) = ClassTypeDecl $ setIdentifier id classDecl
    setIdentifier id (InterfaceTypeDecl interfaceDecl) = InterfaceTypeDecl $ setIdentifier id interfaceDecl

instance Identified CompilationUnit where
    getIdentifier (CompilationUnit _ _ (firstClassOrInterface : tail)) = getIdentifier firstClassOrInterface
    getIdentifier _ = Ident ""
    setIdentifier id (CompilationUnit a b (firstClassOrInterface : tail)) = CompilationUnit a b (setIdentifier id firstClassOrInterface : tail)
    setIdentifier id c = c

instance Identified Decl where
    getIdentifier (MemberDecl decl) = getIdentifier decl
    getIdentifier InitDecl{}        = Ident ""
    setIdentifier id (MemberDecl decl) = MemberDecl $ setIdentifier id decl
    setIdentifier id i@InitDecl{}      = i

instance Identified ClassType where
    getIdentifier (ClassType ((ident, params) : tail)) = ident
    getIdentifier c                                    = Ident ""
    setIdentifier id (ClassType ((_, params) : tail)) = ClassType ((id, params) : tail)
    setIdentifier id c      = c

instance Identified RefType where
    getIdentifier (ClassRefType c) = getIdentifier c
    getIdentifier _                = Ident ""
    setIdentifier id (ClassRefType c) = ClassRefType (setIdentifier id c)
    setIdentifier id c                = c

instance Identified String where
    getIdentifier = Ident . take 3
    setIdentifier (Ident s) other = s
