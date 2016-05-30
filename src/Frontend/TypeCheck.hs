{- Javalette Compiler, a simple C like language.
 - Copyright, 2016, Björn Tropf, Mazdak Farrokhzad
 -
 - This program is free software; you can redistribute it and/or
 - modify it under the terms of the GNU General Public License
 - as published by the Free Software Foundation; either version 2
 - of the License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 -}

{-|
Module      : Frontend.Typecheck
Description : Type checker for Javalette compiler.
Copyright   : (c) Björn Tropf, 2016
                  Mazdak Farrokhzad, 2016
License     : GPL-2+
Stability   : experimental
Portability : ALL

Type checker for Javalette compiler.
-}
{-# LANGUAGE LambdaCase, MultiWayIf, TupleSections #-}


module Frontend.TypeCheck (
    -- * Modules
    module X,

    -- * Operations
    typeCheck
) where

import Data.Data (Data)

import Data.Traversable
import Data.List
import qualified Data.Graph.Inductive as G
import qualified Utils.GraphFlip      as GF
import qualified Data.IntMap          as IM
import qualified Data.Map             as M

import Control.Arrow
import Control.Monad

import Control.Lens hiding (contexts, Empty)

import qualified Data.Generics.Uniplate.Data as U

import Utils.Function
import Utils.Pointless
import Utils.Foldable
import Utils.Monad
import Utils.Sizeables

import Frontend.Environment as X
import Frontend.Error
import Frontend.TypeFunSig
import Frontend.TypeInfer
import Frontend.ReturnCheck

import Common.AST
import Common.ASTOps

u = undefined

--------------------------------------------------------------------------------
-- API:
--------------------------------------------------------------------------------

typeCheck :: Program () -> TCComp ProgramA
typeCheck prog0 = do
    let prog1 = fmap (const emptyAnot) prog0
    -- Part 1: Collect all typedefs
    tinfo "Collecting typedefs"
    collectTypedefs prog1
    -- Part 2: Collect all structs + classes
    tinfo "Collecting structs and classes"
    collectComplex prog1
    -- Part 3: Expand typedefs
    tinfo   "Expanding typedefs"
    prog2 <- expandTypedefs >=> nukeTypedefs $ prog1
    -- Part 4: Collect all functions
    tinfo   "Collecting all functions"
    prog3 <- allFunctions prog2
    tinfo   "AST after collect"
    tinfo $ show prog3
    -- Part 5: Check for the correctness of the main definition
    tinfo   "Checking existence of int main()"
    mainCorrect
    -- Part 6: Type check the program
    tinfo   "Type checking the program"
    prog4 <- checkProg prog3
    checkClasses
    -- Part 7: Return check the program.
    info ReturnChecker "Return checking the program"
    returnCheck prog4

--------------------------------------------------------------------------------
-- Typedefs:
--------------------------------------------------------------------------------

collectTypedefs :: ProgramA -> TCComp ()
collectTypedefs = progCollect _TTypeDef $ \(TypeDef _ typ alias) ->
    fst <$> inferType typ >>= extendTypeName typeIsReserved alias

expandTypedefs :: ProgramA -> TCComp ProgramA
expandTypedefs = (expandInStructs >> expandInClasses) >>. expandTDs

expandInStructs :: TCComp ()
expandInStructs = structs %>= expandTDs

expandInClasses :: TCComp ()
expandInClasses = classGraph %>= \(x, gr) -> (x,) <$> GF.gaction gr expandTDs

expandTDs :: Data from => from -> TCComp from
expandTDs = U.transformBiM $ \case
    typ@(TRef _ alias)   -> expandTD typ alias
    x                    -> return x

expandTD :: TypeA -> Ident -> TCComp TypeA
expandTD typ alias = lookupTypeName noSuchTypeName alias >>= \case
    typ'@(TRef _ alias') -> if typ == typ' then return typ
                            else expandTD typ' alias'
    x                    -> return x

nukeTypedefs :: ProgramA -> TCComp ProgramA
nukeTypedefs = pTopDefs %%~ return . filter (isn't _TTypeDef)

--------------------------------------------------------------------------------
-- Structs:
--------------------------------------------------------------------------------

collectComplex :: ProgramA -> TCComp ()
collectComplex = collectStructs <=>
                 collectClasses .>> (checkVirtual >> reindexClassProps)

collectStructs :: ProgramA -> TCComp ()
collectStructs = progCollect _TStructDef $ \(StructDef _ name fields) ->
    checkFields structDupFields name fields
    >>= extendStruct typeIsReserved (appConcrete $ flip TStruct name) name

checkFields :: (t -> [SFieldA] -> [SFieldA] -> TCComp ())
            ->  t -> [SFieldA] -> TCComp [SFieldA]
checkFields onErr name fields =
    forM fields (sfType %%~ (inferType >$> fst))
    <<= checkNoDups onErr _sfIdent name

checkNoDups :: (Eq a, Applicative f)
             => (t -> [x] -> [x] -> f ()) -> (x -> a) -> t -> [x] -> f ()
checkNoDups onErr xname tname xs =
    let (nubbed, dups) = nubDupsBy ((==) |. xname) xs
    in unless (null dups) (onErr tname nubbed dups)

--------------------------------------------------------------------------------
-- Classes:
--------------------------------------------------------------------------------

collectClasses :: ProgramA -> TCComp ()
collectClasses = computeClassDAG >=> (classGraph .=)

computeClassDAG :: ProgramA -> TCComp ClassDAG
computeClassDAG prog = do
    (cmap, (cgni, lnodes)) <- (id &&& makeCGTNN) <$> computeCIMap prog
    ledges <- foldM (cgBindInherit cmap cgni) [] (snd <$> cmap)
    let graph  = GF.mkGraph lnodes ledges
    let cycles = snd <$$> GF.cyclesIn graph
    unless (null cycles) (cycleDetectedInClasses cycles)
    return (cgni, graph)

makeCGTNN :: [(Ident, ClassInfo)] -> (ClassToCGNode, [CGNode])
makeCGTNN = flip fromKVL [0..] . fmap fst &&& zip [0..] . fmap snd

cgBindInherit :: [(Ident, ClassInfo)] -> ClassToCGNode
              -> [CGEdge] -> ClassInfo -> TCComp [CGEdge]
cgBindInherit cmap cgni edges cl =
    let (name, hier) = _ciIdent &&& _ciHierarchy $ cl
    in  flip (maybe $ return edges) hier $ \s -> do
        when (name == s) (selfCycleDetected cl)
        super <- maybeErr (noSuchSuperClass name s) (lookup s cmap)
        -- swap (from, to)?
        return $ (cgni M.! name, cgni M.! _ciIdent super, ()) : edges

computeCIMap :: ProgramA -> TCComp [(Ident, ClassInfo)]
computeCIMap = intoProg _TClassDef $ \(ClassDef _ name hier parts_) -> do
    props   <- checkFields    classDupProps   name (sndsOfPrism _ClassProp parts_)
    methods <- collectMethods classDupMethods name (sndsOfPrism _MethodDef parts_)
    extendTypeName typeIsReserved name (appConcrete $ flip TRef name)
    return (name, ClassInfo
           { _ciIdent     = name
           , _ciFields    = props
           , _ciFieldsDer = []
           , _ciMethods   = M.fromList $ (_fIdent &&& id) <$> methods
           , _ciHierarchy = hier ^? chIdent })

collectMethods :: (t -> [FnDefA] -> [FnDefA] -> TCComp ())
             ->  t -> [FnDefA] -> TCComp [FnDefA]
collectMethods onErr name methods = do
    checkNoDups onErr _fIdent name methods
    mapM checkFunSignature methods

checkClasses :: TCComp ()
checkClasses = classGraph %>= \(convs, gr) ->
    (convs,) <$> mapM checkMethods gr

checkMethods :: ClassInfo -> TCComp ClassInfo
checkMethods cl = do
    inClass .= True
    cltyp <- lookupTypeName (error "checkMethods: never happens") $ _ciIdent cl
    sInScope contexts $ do
        mapM_ (extendProp cl) $ _ciFields cl
        mapM_ (extendSelf cltyp) prereservedIdents
        (ciMethods %%~ mapM checkFun $ cl) <* (inClass .= False)

--------------------------------------------------------------------------------
-- Classes (Fields):
--------------------------------------------------------------------------------

reindexClassProps :: TCComp ()
reindexClassProps = classGraph %>= \(convs, gr) ->
    fmap ((convs,) . GF.reconstr gr) $ GF.rootBfsM gr [] [] $ \ns ancs child ->
     -- assume single inheritance:
    let der = indexSFields 0 $ ancs >>= (_ciFields . GF.lab' gr)
        clu = (ciFields %~ indexSFields (genericLength der)) .
              (ciFieldsDer %~ const der)
    in return $ ns ++ [(child, clu $ GF.lab' gr child)]

indexSFields :: Integer -> [SField a0] -> [SField a0]
indexSFields x = zipWith (sfIndex .~) [x..]

--------------------------------------------------------------------------------
-- Classes (Virtual):
--------------------------------------------------------------------------------

checkVirtual :: TCComp ()
checkVirtual = classGraph %>= \(convs, gr) ->
                    (convs,) . annotateCGVirt gr <$> computeVirtMap gr

annotateCGVirt :: ClassGraph -> VirtMap -> ClassGraph
annotateCGVirt gr vmap = GF.lnmap gr nm
    where nm (n, cl) = let giv = (M.!) .| (IM.!)
                           xmap m = addVirt m $ giv vmap n $ _fIdent m
                       in (n, ciMethods %~ fmap xmap $ cl)

type VirtMap   = IM.IntMap (M.Map Ident IsVirtual)

computeVirtMap :: ClassGraph -> TCComp VirtMap
computeVirtMap gr =
    let vinit = IM.fromList . fmap (second (fmap (const False) . _ciMethods))
    in GF.rootBfsM gr (vinit $ GF.labNodes gr) [] $ \vmap0 ancs child ->
    foldM' vmap0 ancs $ \vmap1 anc ->
        let ((ln, lms), (rn, rms)) =
                ((_ciIdent &&& _ciMethods) . GF.lab' gr) § (child, anc)
        in foldM2 vmap1 lms rms $ \vmapC fun pfun ->
            if | ((/=) |. _fIdent)   fun pfun -> return vmapC
               | ((==) |. toFnSigId) fun pfun -> markVirtual child fun vmapC >>=
                                                 markVirtual anc   pfun
               | otherwise -> mismatchedOverridenMethod ln fun rn pfun

markVirtual :: G.Node -> FnDefA -> VirtMap -> TCComp VirtMap
markVirtual n f = pure . IM.insertWith M.union n (M.singleton (_fIdent f) True)

--------------------------------------------------------------------------------
-- Type checking:
--------------------------------------------------------------------------------

checkProg :: ProgramA -> TCComp ProgramA
checkProg = pTopDefs %%~ mapM . toFnDef %%~ checkFun

checkFun :: FnDefA -> TCComp FnDefA
checkFun fun = sPushM contexts >> mapM_ extendArg (_fArgs fun) >>
               (fBlock %%~ checkBlock (_fRetTyp fun) $ fun)

checkBlock :: TypeA -> BlockA -> TCComp BlockA
checkBlock rtyp block = (bStmts %%~ mapM (checkStm rtyp) $ block) <*
                        checkUnused <* sPopM contexts

checkUnused :: TCComp ()
checkUnused = view (compileFlags . noWarnUnused) >>=
              flip unless (currUnused >>= mapM_ unusedVar)

checkStm :: TypeA -> StmtA -> TCComp StmtA
checkStm typ stmt = case stmt of
    Empty     _ -> return stmt
    BStmt    {} -> sPushM contexts >> (sBlock %%~ checkBlock typ $ stmt)
    Decl     {} -> checkDecls stmt
    Assign   {} -> checkAssign stmt
    SExp     {} -> sExpr %%~ fmap fst . inferExp $ stmt
    Ret      {} -> sExpr %%~ checkExp typ $ stmt
    VRet      _ -> checkVoid typ >> return stmt
    While    {} -> checkC stmt
    Cond     {} -> checkC stmt
    CondElse {} -> checkC >=> checkS sSe $ stmt
    For      {} -> checkFor typ stmt
    where checkC   = sExpr %%~ checkExp bool >=> checkS sSi
          checkS f = f %%~ checkStm typ

checkFor :: TypeA -> StmtA -> TCComp StmtA
checkFor rtyp = sTyp %%~ (inferType >$> fst) >=> \for_ ->
    let typ = _sTyp for_
    in (sExpr %%~ checkExp (grow typ)) for_ >>= sInScope contexts .
        (extendLocal typ (_sIdent for_) >>) . (sSi %%~ checkStm rtyp)

checkAssign :: StmtA -> TCComp StmtA
checkAssign ass = do
    (lval, ltyp) <- inferLVal $ _sLVal ass
    sExpr %%~ checkExp ltyp $ ass { _sLVal = lval }

checkVoid :: TypeA -> TCComp ()
checkVoid frtyp = unless (frtyp == tvoid) (wrongRetTyp tvoid frtyp)

checkDecls :: StmtA -> TCComp StmtA
checkDecls decl = do
    (vtyp', _) <- inferType $ _sDTyp decl
    sDItems %%~ mapM (single vtyp') $ decl { _sDTyp = vtyp' }
    where single vt it = checkDeclItem vt it <* extendLocal vt (_iIdent it)

checkDeclItem :: TypeA -> ItemA -> TCComp ItemA
checkDeclItem vtyp item = do
    let name = _iIdent item
    iclass <- use inClass
    when (iclass && name `elem` prereservedIdents) $ canNotRedeclareIdent name
    case item of
        NoInit {} -> return item
        _         -> iExpr %%~ checkExp vtyp $ item

extendArg :: ArgA -> TCComp ()
extendArg a = extendVar' argAlreadyDef $ Var (_aIdent a) (_aTyp a) VSArg 0

extendLocal :: TypeA -> Ident -> TCComp ()
extendLocal typ name = extendVar' varAlreadyDef $ Var name typ VSLocal 0

extendProp :: ClassInfo -> SFieldA -> TCComp ()
extendProp cl sf = extendVar' (error "extendProp: should never happen.")
                        $ Var (_sfIdent sf) (_sfType sf)
                              (VSProp $ _ciIdent cl) 0

extendSelf :: TypeA -> Ident -> TCComp ()
extendSelf typ name = extendVar' varAlreadyDef $ Var name typ VSThis 0