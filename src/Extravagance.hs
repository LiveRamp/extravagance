module Extravagance where

import           Accessors
import           Data.Data
import           Data.Generics
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Debug.Trace
import           Language.Java.Lexer
import           Language.Java.Parser
import           Language.Java.Pretty
import           Language.Java.Syntax


(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&) p1 p2 a = p1 a && p2 a

stripSuffix :: String -> String -> String
stripSuffix suffix (s : tail) | suffix == tail = [s]
                                | otherwise = s : stripSuffix suffix tail
stripSuffix suffix [] = []

data MethodPatch = MethodPatch {
    targetName      :: String,
    methodsToInsert :: [MemberDecl],
    fieldsToInsert  :: [MemberDecl]
} deriving (Show)

data InterfacePatch = InterfacePatch {
    targetNameI       :: String,
    interfaceToInsert :: ClassType
} deriving (Show)

data PatchDescription = MP MethodPatch | IP InterfacePatch deriving (Show)

newtype PatchSet =  PatchSet (M.Map String [PatchDescription]) deriving (Show)

instance Semigroup PatchSet where
    (<>) (PatchSet m1) (PatchSet m2) = PatchSet $ M.unionWith (++) m1 m2
instance Monoid PatchSet where
    mempty = PatchSet M.empty

classNamed :: String -> Type
classNamed s = RefType $ ClassRefType $ ClassType [(Ident s, [])]

patchedAnnotation :: InterfaceDecl
patchedAnnotation = InterfaceDecl InterfaceAnnotation [Private, Static] (Ident "Patched") [] [] (InterfaceBody [
    MethodDecl [] [] (Just $ classNamed "String") (Ident "source") [] [] Nothing (MethodBody Nothing) ])

patchedAnnotationDecl :: Decl
patchedAnnotationDecl = MemberDecl $ MemberInterfaceDecl patchedAnnotation

toPatchedAnnotation :: String -> Annotation
toPatchedAnnotation sourceName = NormalAnnotation (Name [getIdentifier patchedAnnotation])
  [(Ident "source", EVVal $ InitExp $ Lit $ String sourceName)]

selectMemberDecls :: MemberDecl -> [MemberDecl]
selectMemberDecls decl = [decl]

isMethod :: MemberDecl -> Bool
isMethod MethodDecl{} = True
isMethod _            = False

isField :: MemberDecl -> Bool
isField FieldDecl{} = True
isField _           = False

getMemberDecls :: CompilationUnit -> [MemberDecl]
getMemberDecls = everything (++) (mkQ [] selectMemberDecls)

isStatic :: Modifier -> Bool
isStatic Static = True
isStatic _      = False

memberIsStatic :: Modified m => m -> Bool
memberIsStatic m = any isStatic (getModifiers m)

removeStatic :: Modified m => m -> m
removeStatic = mapModifiers (filter (not . isStatic))

modifyName :: ([String] -> [String]) -> Name -> Name
modifyName transform (Name idents) = Name $ mapOverIdent idents where
    mapOverIdent = map Ident . transform . map identString

replaceMemberOwnerName :: String -> String -> [String] -> [String]
replaceMemberOwnerName from to (ownerName : tail) | ownerName == from = to : tail
                                                    | otherwise = ownerName : tail
replaceMemberOwnerName from to [] = []

varDeclName :: VarDeclId -> String
varDeclName (VarId (Ident s)) = s
varDeclName (VarDeclArray v)  = varDeclName v

alterVarDeclName :: (String -> String) -> VarDecl -> VarDecl
alterVarDeclName f (VarDecl id initializer) =  VarDecl (alterVarId f id) initializer where
    alterVarId fn (VarId (Ident s))   = VarId (Ident (f s))
    alterVarId f (VarDeclArray varId) = VarDeclArray $ alterVarId f varId

nameOfFormalParam :: FormalParam -> String
nameOfFormalParam (FormalParam _ _ _ v) = varDeclName v

removeNamedParam :: String -> MemberDecl -> MemberDecl
removeNamedParam s (MethodDecl ms ts t i params et mayExp mb) =
    MethodDecl ms ts t i newparams et mayExp mb
            where
                filterfn x = nameOfFormalParam x == s
                newparams = filter (not . filterfn) params
removeNamedParam s m = m

changeFieldName :: (String -> String) -> MemberDecl -> MemberDecl
changeFieldName f (FieldDecl mods t vardecls) = FieldDecl mods t (map (alterVarDeclName f) vardecls)
changeFieldName f m = m


methodHasSelfParam :: MemberDecl -> Bool
methodHasSelfParam = everything (||) (mkQ False selector) where
    selector param = nameOfFormalParam param == "self"

selectMethodsToPatch :: [MemberDecl] -> [MemberDecl]
selectMethodsToPatch = filter (memberIsStatic .&& methodHasSelfParam .&& isMethod)

addPatchedAnnotation :: Modified m => String -> m -> m
addPatchedAnnotation sourceName = mapModifiers ((:) (Annotation $ toPatchedAnnotation sourceName))

isPatched :: Modified m => m -> Bool
isPatched m = isPatched' (getModifiers m) where
    isPatched' (Annotation (NormalAnnotation (Name [i]) _) : tail) = i == getIdentifier patchedAnnotation
    isPatched' (h : tail) = isPatched' tail
    isPatched' [] = False

patchMethod :: String -> MemberDecl -> MemberDecl
patchMethod sourceName =
        removeNamedParam "self" .
        replaceAllSelfsWithThises .
        addPatchedAnnotation sourceName .
        removeStatic
        where
        replaceAllSelfsWithThises = everywhere (mkT $ modifyName (replaceMemberOwnerName "self" "this"))

nameStartsWith :: String -> MemberDecl -> Bool
nameStartsWith prefix m = prefix `isPrefixOf` getIdentString m

selectFieldsToPatch :: [MemberDecl] -> [MemberDecl]
selectFieldsToPatch = filter (memberIsStatic .&& (not . nameStartsWith "static") .&& isField)

patchField :: MemberDecl -> MemberDecl
patchField = removeStatic

generatePatchSet :: CompilationUnit -> PatchSet
generatePatchSet c | isMethodPatch = generateMethodPatch c
                    | isInterfacePatch = generateInterfacePatch c
                    | otherwise = mempty where
    className = getIdentString c
    isMethodPatch = "Methods" `isSuffixOf` className
    isInterfacePatch = "Interface" `isSuffixOf` className

generateMethodPatch :: CompilationUnit -> PatchSet
generateMethodPatch c = PatchSet $ M.singleton targetName [MP methodPatch] where
    targetName = stripSuffix "Methods" (getIdentString c)
    members = getMemberDecls c
    patchedMethods = map (patchMethod (getIdentString c)) $ selectMethodsToPatch members
    patchedFields = map patchField $ selectFieldsToPatch members
    methodPatch = MethodPatch {targetName = targetName, methodsToInsert =patchedMethods, fieldsToInsert = patchedFields}

generateInterfacePatch :: CompilationUnit -> PatchSet
generateInterfacePatch c = PatchSet $ M.singleton targetName [IP interfacePatch] where
    targetName = stripSuffix "Interface" (getIdentString c)
    thisClass = ClassType [(getIdentifier c, [])]
    interfacePatch = InterfacePatch{targetNameI = targetName, interfaceToInsert = thisClass}

applyPatchSet :: PatchSet -> CompilationUnit -> CompilationUnit
applyPatchSet (PatchSet patchMap) c = result where
    patchFn = combinePatches <$> M.lookup (getIdentString c) patchMap
    patchedUnit = patchFn <*> pure c
    result = fromMaybe c patchedUnit

overwrite :: Identified a => a -> [a] -> [a]
overwrite item (head : tail) | getIdentifier item == getIdentifier head = tail ++ [item]
                                        | otherwise = head : overwrite item tail
overwrite item [] = [item]

(++*) :: Identified a => [a] -> [a] -> [a]
(++*) first = foldl (.) id (map overwrite first)

combinePatches :: [PatchDescription] -> CompilationUnit -> CompilationUnit
combinePatches patches = foldl (.) id (map applyPatch patches)

applyPatch :: PatchDescription -> CompilationUnit -> CompilationUnit
applyPatch (MP methodPatch) =  trace ("Applying Method Patch " ++ targetName methodPatch) (modifyClassBody appendToClass) where
    fieldsAndMethods = map MemberDecl $ fieldsToInsert methodPatch ++ methodsToInsert methodPatch
    appendToClass (ClassBody decls) = ClassBody ((patchedAnnotationDecl : fieldsAndMethods) ++* filter (not . isPatched) decls)
applyPatch (IP interfacePatch) = trace ("Applying Interface Patch " ++ targetNameI interfacePatch) modifyInterfaces appendInterface where
    appendInterface refs = [ClassRefType $ interfaceToInsert interfacePatch] ++* refs

modifyClass :: (ClassDecl -> ClassDecl) -> CompilationUnit -> CompilationUnit
modifyClass m = gmapT (mkT modifyTypeDecls) where
    modifyTypeDecl = gmapT (mkT m) :: TypeDecl -> TypeDecl
    modifyTypeDecls = map modifyTypeDecl :: [TypeDecl] -> [TypeDecl]

modifyClassBody :: (ClassBody -> ClassBody) -> CompilationUnit -> CompilationUnit
modifyClassBody m = modifyClass (gmapT $ mkT m)

modifyInterfaces :: ([RefType] -> [RefType]) -> CompilationUnit -> CompilationUnit
modifyInterfaces m = modifyClass (gmapT $ mkT m)
