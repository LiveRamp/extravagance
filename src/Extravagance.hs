module Extravagance where

import           Accessors
import           Util
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

applyPatchSet :: PatchSet -> CompilationUnit -> CompilationUnit
applyPatchSet (PatchSet patchMap) c = result where
    patchFn = combinePatches <$> M.lookup (getIdentString c) patchMap
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

combinePatches :: [PatchDescription] -> CompilationUnit -> CompilationUnit
combinePatches patches = foldl (.) id (map applyPatch patches)

applyPatch :: PatchDescription -> CompilationUnit -> CompilationUnit
applyPatch (MP methodPatch) =  trace ("Applying Method Patch " ++ targetName methodPatch) $
    modifyDeclList appendToDeclarations . appendImports where
        fieldsAndMethods = map MemberDecl $ fieldsToInsert methodPatch ++ methodsToInsert methodPatch
        imports = importsToInsert methodPatch
        appendToDeclarations decls = (patchedAnnotationDecl : fieldsAndMethods) ++* filter (not . isPatched) decls
        appendImports (CompilationUnit package existingImports body ) = CompilationUnit package (imports ++* existingImports) body
applyPatch (IP interfacePatch) = trace ("Applying Interface Patch " ++ targetNameI interfacePatch) (modifyInterfaces appendInterface) where
    appendInterface refs = [ClassRefType $ interfaceToInsert interfacePatch] *++ refs
applyPatch (PreH preHookPatch) = trace ("Applying PreHook Patch " ++ targetNameP preHookPatch) $ insertPreHook preHookPatch
applyPatch (RP redactionPatch) = trace ("Applying Redaction Patch " ++ targetNameR redactionPatch) id -- TODO

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

modifyMethodStatments :: ([BlockStmt] -> [BlockStmt]) -> MemberDecl -> MemberDecl
modifyMethodStatments fn (MethodDecl a b c d e f g (MethodBody (Just (Block stmts)))) =
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
    patcher m = (modifyMethodStatments . prepender) m m
    cleaner = modifyMethodStatments removeExistingPrehook
    insertFn = modifyIf (preHookMatch patch) (patcher . cleaner)

redactBlockStmt :: RedactionPatch -> BlockStmt -> BlockStmt
redactBlockStmt patch (LocalVars modifiers varType varDecls) =
    LocalVars modifiers varType $ map redactVarDecl varDecls
    where redactVarDecl decl@(VarDecl declId varInit) =
            case varInit of
                Nothing -> decl
                Just varInit -> VarDecl declId $ Just $ redactVarInit patch varInit
-- TODO handle other statements

redactBlock :: RedactionPatch -> Block -> Block
redactBlock patch (Block stmts) = Block $ map (redactBlockStmt patch) stmts

redactExp :: RedactionPatch -> Exp -> Exp
-- redact direct field accesses
redactExp (RedactionPatch target) fieldAccess@(FieldAccess (PrimaryFieldAccess exp (Ident name)))
    | target == name = Lit $ String "redacted"
    | otherwise      = fieldAccess
-- If a field is called some_field, and the reference is just to "some_field"
-- and not "this.some_field", it is parsed as an ExpName rather than a FieldAccess
redactExp (RedactionPatch target) expName@(ExpName (Name [Ident name]))
    | target == name = Lit $ String "redacted"
    | otherwise      = expName
redactExp _ expName@(ExpName _) = expName

-- redact constructor calls
redactExp patch (InstanceCreation a b args c) = InstanceCreation a b (redactExps patch args) c
redactExp patch (QualInstanceCreation exp a b args c) = QualInstanceCreation (redactExp patch exp) a b (redactExps patch args) c

-- redact array creations
-- ArrayCreate intentionally not modified (e.g. new int[some_field.length()] would not be changed to new int["redacted".length()])
redactExp patch (ArrayCreateInit a b arrayInit) = ArrayCreateInit a b $ redactArrayInit patch arrayInit

-- redact method calls
redactExp patch (MethodInv (MethodCall a args)) = MethodInv $ MethodCall a $ redactExps patch args
-- this is the one that really matters, since thrift uses sb.append(this.whatever_field)
redactExp patch (MethodInv (PrimaryMethodCall exp a b args)) = MethodInv $ PrimaryMethodCall (redactExp patch exp) a b (redactExps patch args)
redactExp patch (MethodInv (SuperMethodCall a b args)) = MethodInv $ SuperMethodCall a b $ redactExps patch args
redactExp patch (MethodInv (ClassMethodCall a b c args)) = MethodInv $ ClassMethodCall a b c $ redactExps patch args
redactExp patch (MethodInv (TypeMethodCall a b c args)) = MethodInv $ TypeMethodCall a b c $ redactExps patch args

redactExp patch (Cast a exp) = Cast a $ redactExp patch exp

-- redact non-boolean binOps
-- i.e. keep things like this.some_field == null
redactExp patch binOp@(BinOp lhs op rhs)
    | op `elem` [LThan, GThan, LThanE, GThanE, Equal, NotEq] = binOp
    | otherwise = BinOp (redactExp patch lhs) op (redactExp patch rhs)

redactExp patch (Cond a yes no) = Cond a (redactExp patch yes) (redactExp patch no)
redactExp patch (Assign lhs op rhs) = Assign lhs op $ redactExp patch rhs
redactExp patch (Lambda params lambdaExpression) = Lambda params $
    case lambdaExpression of
        LambdaExpression exp -> LambdaExpression $ redactExp patch exp
        LambdaBlock block -> LambdaBlock $ redactBlock patch block

redactExp patch a = a

redactExps :: RedactionPatch -> [Exp] -> [Exp]
redactExps patch = map (redactExp patch)

redactVarInit :: RedactionPatch -> VarInit -> VarInit
redactVarInit patch init = case init of
    InitExp exp -> InitExp $ redactExp patch exp
    InitArray arrayInit -> InitArray $ redactArrayInit patch arrayInit

redactArrayInit :: RedactionPatch -> ArrayInit -> ArrayInit
redactArrayInit patch (ArrayInit varInits) = ArrayInit $ map (redactVarInit patch) varInits
