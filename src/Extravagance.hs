{-# LANGUAGE DeriveGeneric #-}
module Extravagance where

import           Accessors
import           Util
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as C
import           Data.Data
import           Data.Generics
import           Data.List
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Debug.Trace
import qualified GHC.Generics         as G
import           Language.Java.Lexer
import           Language.Java.Parser
import           Language.Java.Pretty
import           Language.Java.Syntax

data MethodPatch = MethodPatch {
    targetName      :: String,
    methodsToInsert :: [MemberDecl],
    fieldsToInsert  :: [MemberDecl],
    importsToInsert :: [ImportDecl]
} deriving (Show)

data PreHookPatch = PreHookPatch {
    targetNameP  :: String,
    methodToHook :: String,
    hookCall     :: MemberDecl
} deriving (Show)

data InterfacePatch = InterfacePatch {
    targetNameI       :: String,
    interfaceToInsert :: ClassType
} deriving (Show)

data RedactionPatch = RedactionPatch {
    targetNameR :: String
} deriving (Show)

data PatchDescription = MP MethodPatch | IP InterfacePatch | PreH PreHookPatch | RP RedactionPatch deriving (Show)

-- A Map from class name to the patches that should be applied to that class
-- Class name should be unqualified (e.g. "String", not "java.lang.String")
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
selectMethodsToPatch = filter (memberIsStatic .&& isMethod)

addPatchedAnnotation :: Modified m => String -> m -> m
addPatchedAnnotation sourceName = mapModifiers ((:) (Annotation $ toPatchedAnnotation sourceName))

isPatched :: Modified m => m -> Bool
isPatched m = isPatched' (getModifiers m) where
    isPatched' (Annotation (NormalAnnotation (Name [i]) _) : tail) = i == getIdentifier patchedAnnotation
    isPatched' (h : tail) = isPatched' tail
    isPatched' [] = False

patchMethod :: String -> MemberDecl -> MemberDecl
patchMethod sourceName =
        modifyIf methodHasSelfParam
        (removeNamedParam "self" .
        replaceAllSelfsWithThises .
        addPatchedAnnotation sourceName .
        removeStatic)
        where
        replaceAllSelfsWithThises = everywhere (mkT $ modifyName (replaceMemberOwnerName "self" "this"))

nameStartsWith :: String -> MemberDecl -> Bool
nameStartsWith prefix m = prefix `isPrefixOf` getIdentString m

selectFieldsToPatch :: [MemberDecl] -> [MemberDecl]
selectFieldsToPatch = filter (memberIsStatic .&& (not . nameStartsWith "static") .&& isField)

patchField :: MemberDecl -> MemberDecl
patchField = removeStatic

isPreHook :: MemberDecl -> Bool
isPreHook m@(MethodDecl _ _ _ _ params _ _ _) =
    "pre_" `isPrefixOf` getIdentString m &&
    memberIsStatic m &&
    methodHasSelfParam m
isPreHook _ = False

createPreHookInvocation :: String -> MemberDecl -> PreHookPatch
createPreHookInvocation targetName m = PreHookPatch targetName methodToHook m where
    methodToHook = fromJust $ stripPrefix "pre_" (getIdentString m)

generatePatchSet :: CompilationUnit -> PatchSet
generatePatchSet c  | isMethodPatch = generateMethodPatch c `mappend` generatePreHookPatch c
                    | isInterfacePatch = generateInterfacePatch c
                    | otherwise = mempty where
    className = getIdentString c
    isMethodPatch = "Methods" `isSuffixOf` className
    isInterfacePatch = "Interface" `isSuffixOf` className

generateMethodPatch :: CompilationUnit -> PatchSet
generateMethodPatch c = PatchSet $ M.singleton targetName [MP methodPatch] where
    targetName = stripSuffix "Methods" (getIdentString c)
    imports = getImportDecls c
    members = getMemberDecls c
    patchedMethods = map (patchMethod (getIdentString c)) $ selectMethodsToPatch members
    patchedFields = map patchField $ selectFieldsToPatch members
    methodPatch = MethodPatch {targetName = targetName,
                               methodsToInsert = patchedMethods,
                               fieldsToInsert = patchedFields,
                               importsToInsert = imports}

generateInterfacePatch :: CompilationUnit -> PatchSet
generateInterfacePatch c = PatchSet $ M.singleton targetName [IP interfacePatch] where
    targetName = stripSuffix "Interface" (getIdentString c)
    thisClass = ClassType [(getIdentifier c, [])]
    interfacePatch = InterfacePatch{targetNameI = targetName, interfaceToInsert = thisClass}

generatePreHookPatch :: CompilationUnit -> PatchSet
generatePreHookPatch c = PatchSet $ M.singleton targetName (map PreH preHookMethods) where
    targetName = stripSuffix "Methods" (getIdentString c)
    members = getMemberDecls c
    preHookMethods = map (createPreHookInvocation targetName) $ filter isPreHook members

data JsonPatchSet = JsonPatchSet {
    sensitiveFields :: M.Map String [String]
} deriving (Show, G.Generic)
instance A.FromJSON JsonPatchSet
instance A.ToJSON JsonPatchSet

generateJsonPatchSet :: B.ByteString -> PatchSet
generateJsonPatchSet fileContents = case (A.eitherDecode fileContents :: Either String JsonPatchSet) of
    Left err -> trace err $ PatchSet M.empty
    Right (JsonPatchSet sensitiveFieldPatches) -> PatchSet $ M.map (map (RP . RedactionPatch)) sensitiveFieldPatches

applyPatchSet :: Resources -> PatchSet -> CompilationUnit -> CompilationUnit
applyPatchSet r (PatchSet patchMap) c = result where
    patchFn = combinePatches r <$> M.lookup (getIdentString c) patchMap
    patchedUnit = patchFn <*> pure c
    result = fromMaybe c patchedUnit

identNotIn :: Identified a => a -> [a] -> Bool
identNotIn a = all (\x -> getIdentifier x /= getIdentifier a)

overwriteHead :: Identified a => a -> [a] -> [a]
overwriteHead item ls = if identNotIn item ls then item : ls else ls

overwriteTail :: Identified a => a -> [a] -> [a]
overwriteTail item ls = if identNotIn item ls then ls ++ [item] else ls

(++*) :: Identified a => [a] -> [a] -> [a]
(++*) first = foldl (.) id (map overwriteTail first)

(*++) :: Identified a => [a] -> [a] -> [a]
(*++) first = foldl (.) id (map overwriteHead first)

combinePatches :: Resources -> [PatchDescription] -> CompilationUnit -> CompilationUnit
combinePatches r patches = foldl (.) id (map (applyPatch r) patches)

data Resources = Resources {
  sensitiveFieldDecl :: MemberDecl,
  redactingToStringDecl :: MemberDecl
}

parseMemberDecl :: String -> MemberDecl
parseMemberDecl string = case parser compilationUnit string of
    Right decl -> case getMember decl of
        Just d -> d
        where getMember = something (mkQ Nothing (\x -> Just (x :: MemberDecl)))

applyPatch :: Resources -> PatchDescription -> CompilationUnit -> CompilationUnit
applyPatch _ (MP methodPatch) =  trace ("Applying Method Patch " ++ targetName methodPatch) $
    modifyDeclList appendToDeclarations . appendImports where
        fieldsAndMethods = map MemberDecl $ fieldsToInsert methodPatch ++ methodsToInsert methodPatch
        imports = importsToInsert methodPatch
        appendToDeclarations decls = (patchedAnnotationDecl : fieldsAndMethods) ++* filter (not . isPatched) decls
        appendImports (CompilationUnit package existingImports body ) = CompilationUnit package (imports ++* existingImports) body
applyPatch _ (IP interfacePatch) = trace ("Applying Interface Patch " ++ targetNameI interfacePatch) (modifyInterfaces appendInterface) where
    appendInterface refs = [ClassRefType $ interfaceToInsert interfacePatch] *++ refs
applyPatch _ (PreH preHookPatch) = trace ("Applying PreHook Patch " ++ targetNameP preHookPatch) $ insertPreHook preHookPatch
applyPatch (Resources sensitiveFieldDecl redactingToStringDecl) (RP redactionPatch) = trace ("Applying toString Redaction Patch " ++ targetNameR redactionPatch) (redactMethod redactionPatch . redactUnion sensitiveFieldDecl redactingToStringDecl redactionPatch)

modifyClass :: (ClassDecl -> ClassDecl) -> CompilationUnit -> CompilationUnit
modifyClass m = gmapT (mkT modifyTypeDecls) where
    modifyTypeDecl = gmapT (mkT m) :: TypeDecl -> TypeDecl
    modifyTypeDecls = map modifyTypeDecl :: [TypeDecl] -> [TypeDecl]

modifyClassBody :: (ClassBody -> ClassBody) -> CompilationUnit -> CompilationUnit
modifyClassBody m = modifyClass (gmapT $ mkT m)

modifyEnumBody :: (EnumBody -> EnumBody) -> CompilationUnit -> CompilationUnit
modifyEnumBody m = modifyClass (gmapT $ mkT m)

modifyDeclList :: ([Decl] -> [Decl]) -> CompilationUnit -> CompilationUnit
modifyDeclList m = modifyClassBody (gmapT $ mkT m) . modifyEnumBody (gmapT $ mkT m)

modifyInterfaces :: ([RefType] -> [RefType]) -> CompilationUnit -> CompilationUnit
modifyInterfaces m = modifyClass (gmapT $ mkT m)

modifyMethodStatements :: ([BlockStmt] -> [BlockStmt]) -> MemberDecl -> MemberDecl
modifyMethodStatements fn (MethodDecl a b c d e f g (MethodBody (Just (Block stmts)))) =
    MethodDecl a b c d e f g (MethodBody (Just (Block (fn stmts))))

makeMethodCall :: MemberDecl -> MemberDecl -> BlockStmt
makeMethodCall hook container = BlockStmt $ ExpStmt $ MethodInv invocation where
    paramExps = map ExpName $ getParamNames container
    invocation = MethodCall (Name [getIdentifier hook]) paramExps

preHookMatch :: PreHookPatch -> MemberDecl -> Bool
preHookMatch patch m = nameMatch && paramMatch where
    nameMatch = getIdentString m == methodToHook patch
    -- tail here because method is unpatched, so first arg is "self"
    paramMatch = tail (getParamTypes (hookCall patch)) == getParamTypes m

removeExistingPrehook :: [BlockStmt] -> [BlockStmt]
removeExistingPrehook = filter (not . isPreHookCall) where
    isPreHookCall (BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident s]) _)))) = "pre_" `isPrefixOf` s
    isPreHookCall a = False

insertPreHook :: PreHookPatch -> CompilationUnit -> CompilationUnit
insertPreHook patch = everywhere (mkT insertFn) where
    stmtMaker = makeMethodCall (hookCall patch)
    prepender m b = stmtMaker m : b
    patcher m = (modifyMethodStatements . prepender) m m
    cleaner = modifyMethodStatements removeExistingPrehook
    insertFn = modifyIf (preHookMatch patch) (patcher . cleaner)

redactMethodMatch :: MemberDecl -> Bool
redactMethodMatch m = getIdentString m == "toString" && getParams m == []

redactMethod :: RedactionPatch -> CompilationUnit -> CompilationUnit
redactMethod patch = everywhere (mkT $ modifyIf redactMethodMatch patchMethod) where
    patchMethod = modifyMethodStatements (map (redactBlockStmt patch))

redactBlockStmt :: RedactionPatch -> BlockStmt -> BlockStmt
redactBlockStmt patch = everywhereBut (mkQ False isComparisonBinOp) (mkT (redactExp patch))
    -- we want to leave comparisons untouched (e.g. some_field == null should _not_ be replaced by "<redacted>" == null)
    where isComparisonBinOp (BinOp lhs op rhs) = op `elem` [LThan, GThan, LThanE, GThanE, Equal, NotEq]
          isComparisonBinOp _ = False

redactExp :: RedactionPatch -> Exp -> Exp
redactExp patch@(RedactionPatch target) exp = case exp of
    -- direct field accesses
    fieldAccess@(FieldAccess (PrimaryFieldAccess exp (Ident name))) -> if target == name then redactedExp else fieldAccess
    -- If a field is called some_field, and the reference is just to "some_field"
    -- and not "this.some_field", it is parsed as an ExpName rather than a FieldAccess
    expName@(ExpName (Name [Ident name])) -> if target == name then redactedExp else expName
    expName@(ExpName _) -> expName
    _ -> exp
    where redactedExp = Lit $ String "<redacted>"

redactUnion :: MemberDecl -> MemberDecl -> RedactionPatch -> CompilationUnit -> CompilationUnit
redactUnion sensitiveFieldsDecl redactingToStringDecl patch = everywhere (mkT $ modifyIf isUnion patchMethod) where
    patchMethod = insertOrUpdateSensitiveFieldList sensitiveFieldsDecl patch . insertRedactingToStringIfNeeded redactingToStringDecl

isUnion :: CompilationUnit -> Bool
isUnion = hasAny isUnionClassDecl where
    isUnionClassDecl c@(ClassDecl _ _ _ (Just (ClassRefType (ClassType [(Ident "org", []), (Ident "apache", []), (Ident "thrift", []), (Ident "TUnion", _)]))) _ _) = True
    isUnionClassDecl _ = False

insertRedactingToStringIfNeeded :: MemberDecl -> CompilationUnit -> CompilationUnit
insertRedactingToStringIfNeeded redactingToStringDecl = modifyIf toStringNotPresent (insertMemberIntoClass redactingToStringDecl) where
    isToStringDecl (MethodDecl _ _ _ (Ident "toString") _ _ _ _) = True
    isToStringDecl _ = False
    toStringNotPresent = not . hasAny isToStringDecl

insertOrUpdateSensitiveFieldList :: MemberDecl -> RedactionPatch -> CompilationUnit -> CompilationUnit
insertOrUpdateSensitiveFieldList sensitiveFieldsDecl patch decl = updateSensitiveFieldList sensitiveFieldsDecl patch $ modifyIf (missingSensitiveFieldList sensitiveFieldsDecl) (insertMemberIntoClass sensitiveFieldsDecl) decl

updateSensitiveFieldList :: MemberDecl -> RedactionPatch -> CompilationUnit -> CompilationUnit
updateSensitiveFieldList sensitiveFieldsDecl (RedactionPatch fieldName) = modifyDeclList (map doEdit) where
    doEdit = modifyIf (isSensitiveFieldList sensitiveFieldsDecl) $ everywhere (mkT $ insertArgToMethodCall newArg) where
        -- The _Fields name is always the union field name uppercased
        -- e.g. foo_bar -> _Fields.FOO_BAR
        newArg = ExpName (Name [Ident "_Fields",Ident (map C.toUpper fieldName)])
        insertArgToMethodCall arg (MethodCall a args) = MethodCall a (arg:args)

insertMemberIntoClass :: MemberDecl -> CompilationUnit -> CompilationUnit
insertMemberIntoClass newMember = modifyDeclList ([MemberDecl newMember] ++)

-- return True iff the class contains a sensitive field list MemberDecl
missingSensitiveFieldList :: MemberDecl -> CompilationUnit -> Bool
missingSensitiveFieldList sensitiveFieldsDecl c = not $ hasAny (isSensitiveFieldList sensitiveFieldsDecl) c

isSensitiveFieldList :: MemberDecl -> Decl -> Bool
isSensitiveFieldList sensitiveFieldsDecl = hasAny isSensitiveFieldsVarId where
    getVarName (VarId (Ident name)) = name
    sensitiveVarId = case something (mkQ Nothing (Just . getVarName)) sensitiveFieldsDecl of
        Just x -> x
    isSensitiveFieldsVarId varId = getVarName varId == sensitiveVarId
