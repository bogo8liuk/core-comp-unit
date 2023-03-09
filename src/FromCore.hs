{- This work is inspired by https://www.stephendiehl.com/posts/ghc_03.html (source code at
https://github.com/sdiehl/dive-into-ghc) -}

module FromCore
    ( coreCompUnit
) where

import Control.Monad.IO.Class(liftIO)
import Data.Set(Set)
import System.IO
import System.Exit(exitFailure)
import Data.Time.Clock
import GHC.Paths(libdir)
import GHC
import HscTypes
import PrelNames
import RdrName
import InstEnv
import FamInstEnv
import CostCentre
import Rules
import Stream hiding (liftIO)
import UniqSupply
import CoreSyn
import CoreOpt
import SimplCore
import TidyPgm
import CorePrep
import CoreMonad
import CoreLint
import CoreToStg
import StgSyn
import SimplStg
import StgFVs
import GHC.StgToCmm
import Cmm
import CmmBuildInfoTables
import CmmPipeline
import CmmInfo
import PprC
import AsmCodeGen
import LlvmCodeGen
import BinIface

data TargetLinkOptsErr =
      InterReqInMem
    | CGenReqNoObj
    | NoGencodeNoLink

infoTrgLinkOpts :: String
infoTrgLinkOpts = "See HscTarget and GhcLink for the available options."

{- TODO: just for now, using Show instance, but type-classes in Lib.Result should be used. -}
instance Show TargetLinkOptsErr where
    show InterReqInMem = "Bytecode generation requires in-memory linking option. " ++ infoTrgLinkOpts
    show CGenReqNoObj = "Generated C code cannot be used as object code and linked. " ++ infoTrgLinkOpts
    show NoGencodeNoLink = "If there is no generated code, cannot link anything. " ++ infoTrgLinkOpts

{- It checks if the target is suitable with linking option: if all ok, it returns Nothing, else it returns a
TargetLinkOptsErr error value. -}
checkTrgLink :: HscTarget -> GhcLink -> Maybe TargetLinkOptsErr
checkTrgLink HscInterpreted LinkInMemory = Nothing
checkTrgLink HscInterpreted _ = Just InterReqInMem
checkTrgLink HscC NoLink = Nothing
{- TODO: not sure about this, is C codegen suitable for object linking??? -}
checkTrgLink HscC _ = Just CGenReqNoObj
{- If there is no generated code, then it cannot try to link object code. -}
checkTrgLink HscNothing NoLink = Nothing
checkTrgLink HscNothing _ = Just NoGencodeNoLink
checkTrgLink HscAsm _ = Nothing
checkTrgLink HscLlvm _ = Nothing

objAllowed :: HscTarget -> Bool
objAllowed HscInterpreted = False
objAllowed HscC = False
objAllowed HscNothing = False
objAllowed HscAsm = True
objAllowed HscLlvm = True

{- Given a string-represented module name and a HscTarget value, it builds the right Target value. -}
mkTarget :: ModuleName -> HscTarget -> Target
mkTarget modName trg =
    Target
        { targetId = TargetModule modName
        {- Exploiting a HscTarget value to determine if object code is allowed. -}
        , targetAllowObjCode = objAllowed trg
        , targetContents = Nothing
        }

mkGuts :: Module -> [TyCon] -> [ClsInst] -> CoreProgram -> ModGuts
mkGuts modl tycons insts cp =
    ModGuts
        { mg_module = modl
        , mg_hsc_src = HsSrcFile
        {- TODO: this is a point in a file, `noSrcSpan` is bad (no info about the position) and can be improved. -}
        , mg_loc = noSrcSpan
        {- TODO: these are the exported symbols. They can passed to the function. -}
        , mg_exports = []
        , mg_deps = noDependencies
        {- TODO: this field is for recompilation avoidance and it determines what was used. This is useful for
        interfaces. Think deeper about it. -}
        , mg_usages = []
        {- TODO: what is it? -}
        , mg_used_th = False
        {- TODO: what is it? -}
        , mg_rdr_env = emptyGlobalRdrEnv
        , mg_fix_env = emptyFixityEnv
        , mg_tcs = tycons
        , mg_insts = insts
        , mg_fam_insts = []
        , mg_patsyns = []
        , mg_rules = []
        , mg_binds = cp
        , mg_foreign = NoStubs
        , mg_foreign_files = []
        , mg_warns = NoWarnings
        , mg_anns = []
        {- TODO: these are the constructors along with the (base) type which they build (e.g. Just, Nothing with
        Maybe). Discover for what is used and eventually fill this field. -}
        , mg_complete_sigs = []
        , mg_hpc_info = NoHpcInfo False
        , mg_modBreaks = Just emptyModBreaks
        {- TODO: what is it? -}
        , mg_inst_env = emptyInstEnv
        {- TODO: what is it? -}
        , mg_fam_inst_env = emptyFamInstEnv
        {- TODO: this can be parameterized. -}
        , mg_safe_haskell = Sf_None
        , mg_trust_pkg = False
        {- TODO: using module name??? -}
        , mg_doc_hdr = Nothing
        , mg_decl_docs = emptyDeclDocMap
        , mg_arg_docs = emptyArgDocMap
        }

mkModLoc :: String -> ModLocation
mkModLoc name =
    ModLocation
        {- There is no hs source. -}
        { ml_hs_file = Nothing
        , ml_hi_file = name ++ ".hi"
        , ml_obj_file = name ++ ".o"
        {- This file is for recompilation avoidance (wheter it exists or it does not). -}
        , ml_hie_file = name ++ ".hie"
        }

mkModLocSrc :: String -> ModLocation
mkModLocSrc name =
    ModLocation
        { ml_hs_file = Just $ name ++ ".hs"
        , ml_hi_file = name ++ ".hi"
        , ml_obj_file = name ++ ".o"
        , ml_hie_file = name ++ ".hie"
        }

mkSumm :: DynFlags -> Module -> ModLocation -> ModSummary
mkSumm dflags modl modLoc =
    ModSummary
        { ms_mod = modl
        , ms_hsc_src = HsSrcFile
        , ms_location = modLoc
        , ms_hs_date = UTCTime (toEnum 0) 0
        , ms_obj_date = Nothing
        , ms_iface_date = Nothing
        , ms_hie_date = Nothing
        , ms_srcimps = []
        , ms_textual_imps = []
        , ms_parsed_mod = Nothing
        {- TODO: maybe using a different parameter is better instead of the module name. -}
        , ms_hspp_file = fetchModlName modl ++ ".hs"
        , ms_hspp_opts = dflags
        , ms_hspp_buf = Nothing
        }

fetchModlName :: Module -> String
fetchModlName = moduleNameString . moduleName

getModlNameFrom :: CgGuts -> Ghc String
getModlNameFrom guts = return . fetchModlName $ cg_module guts

{- This is part of the core-to-core pipeline: it makes just the first simple optimizations on a Core program. -}
coreSimplifier :: ModGuts -> Ghc ModGuts
coreSimplifier guts = do
    dflags <- getSessionDynFlags
    {- TODO: not sure about the call rulesOfBinds to get CoreRule list. -}
    let cp = mg_binds guts
    (cp', rules) <- liftIO . simpleOptPgm dflags (mg_module guts) cp $ rulesOfBinds cp
    {- Changing guts. TODO: verify it -}
    return $ guts
        { mg_binds = cp'
        , mg_rules = rules
        }

{- The core optimizer. This should represent the main core-to-core optimization pipeline. -}
coreOptimizer :: ModGuts -> Ghc ModGuts
coreOptimizer guts = do
    env <- getSession
    liftIO $ core2core env guts

coreTidy :: ModGuts -> Ghc (CgGuts, ModDetails)
coreTidy guts = do
    env <- getSession
    liftIO $ tidyProgram env guts

{- The core type-checker. It is not mandatory to include it in the backend pipeline, but it is useful to grant not
to have seg faults afterwards. -}
coreLint :: CoreToDo -> CoreProgram -> Ghc ()
coreLint todo cp = do
    env <- getSession
    liftIO $ lintPassResult env todo cp

{- It prepares a Core program for Stg code generation. -}
corePrepare :: CgGuts -> Ghc (CgGuts, Set CostCentre)
corePrepare guts = do
    dflags <- getSessionDynFlags
    env <- getSession
    name <- getModlNameFrom guts
    let modLoc = mkModLoc name
    let modl = cg_module guts
    let summ = mkSumm dflags modl modLoc
    (cp, cc) <- liftIO $
        corePrepPgm env modl (ms_location summ) (cg_binds guts) (cg_tycons guts)
    return (guts { cg_binds = cp }, cc)

{- It creates the Stg bindings from Core ones. -}
fromCoreToStg :: CgGuts -> Ghc ([StgTopBinding], CollectedCCs)
fromCoreToStg guts = do
    dflags <- getSessionDynFlags
    let modl = cg_module guts
    let cp = cg_binds guts
    return $ coreToStg dflags modl cp

{- This is an stg-to-stg phase, it does an optimization on Stg program. -}
stgSimplifier :: CgGuts -> [StgTopBinding] -> Ghc [StgTopBinding]
stgSimplifier guts stgp = do
    dflags <- getSessionDynFlags
    let modl = cg_module guts
    liftIO $ stg2stg dflags modl stgp

{- C-- code generation from Stg program. -}
fromStgToCmm :: CgGuts -> [StgTopBinding] -> CollectedCCs -> Ghc (Stream IO CmmGroup ())
fromStgToCmm guts stgp ccs = do
    dflags <- getSessionDynFlags
    let modl = cg_module guts
    let tycons = cg_tycons guts
    let hpcInfo = cg_hpc_info guts
    return $ codeGen dflags modl tycons ccs (annTopBindingsFreeVars stgp) hpcInfo

{- It optimizes a C-- program, making it usable to generate asm. -}
cmmOptimizer :: CgGuts -> Stream IO CmmGroup () -> Ghc (Stream IO CmmGroup ModuleSRTInfo)
cmmOptimizer guts cmmpStream = do
    env <- getSession
    let srts = emptySRT $ cg_module guts
    return $ Stream.mapAccumL (cmmPipeline env) srts cmmpStream

{- Preparing a C-- program for code generation (whatever the target it is). -}
cmmPrepare :: Stream IO CmmGroup ModuleSRTInfo -> Ghc (Stream IO RawCmmGroup ModuleSRTInfo)
cmmPrepare cmmpStream = do
    dflags <- getSessionDynFlags
    liftIO $ cmmToRawCmm dflags cmmpStream

{- According to the target, it generates the final code (it creates a file). -}
fromCmmTo :: HscTarget -> CgGuts -> Stream IO RawCmmGroup ModuleSRTInfo -> Ghc ModuleSRTInfo
fromCmmTo HscC guts rawCmmpStream = do
    (cmmProg, srts) <- liftIO $ collect_ rawCmmpStream
    dflags <- getSessionDynFlags
    name <- getModlNameFrom guts
    liftIO $ mapM_ (writeCFile dflags name) cmmProg
    return srts
    where
        writeCFile dflags name cmmGroup = do
            {- Using AppendMode because this function is called several times. This is due to the fact that there's
            a stream which gives back a C-- program which is just a list of RawCmmGroup tokens. So the multiple calls
            are on the list and what has to be printed has not to overwrite the previous print. -}
            handle <- openFile (name ++ ".c") AppendMode
            writeC dflags handle cmmGroup
fromCmmTo HscAsm guts rawCmmpStream = do
    uniqSup <- liftIO $ mkSplitUniqSupply 'a'
    dflags <- getSessionDynFlags
    name <- getModlNameFrom guts
    liftIO $ writeAsmFile dflags name uniqSup rawCmmpStream
    where
        writeAsmFile dflags name uniqSup stream = do
            handle <- openFile (name ++ ".s") WriteMode
            nativeCodeGen dflags (cg_module guts) (mkModLoc name) handle uniqSup stream
fromCmmTo HscLlvm guts rawCmmpStream = do
    dflags <- getSessionDynFlags
    name <- getModlNameFrom guts
    liftIO $ writeLlvmFile dflags name rawCmmpStream
    where
        writeLlvmFile dflags name stream = do
            handle <- openFile (name ++ ".ll") WriteMode
            llvmCodeGen dflags handle stream
{- In the case of the target is bytecode (HscInterpreted) or there is no code-gen (HscNothing), what happens
is just the evaluation of the stream and nothing more, no action is run. -}
fromCmmTo _ _ rawCmmpStream = do
    (_, srts) <- liftIO $ collect_ rawCmmpStream
    return srts

type MkIface = Maybe ModIface

eventuallyMkIface :: MkIface -> CgGuts -> Ghc ()
eventuallyMkIface (Just intfMod) guts = do
    dflags <- getSessionDynFlags
    name <- getModlNameFrom guts
    liftIO $ writeBinIface dflags (name ++ "hi") intfMod
eventuallyMkIface Nothing _ = return ()

coreCompUnit :: Module -> HscTarget -> GhcLink -> MkIface -> CoreProgram -> [TyCon] -> [ClsInst] -> IO ()
coreCompUnit modl trgOpt linkOpt intfToMk cp tyCons insts =
    case checkTrgLink trgOpt linkOpt of
        Just err -> do
            print err
            exitFailure
        Nothing ->
            runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags $ dflags
                    { hscTarget = trgOpt
                    , ghcLink = linkOpt
                    }

                let modName = moduleName modl
                setTargets [mkTarget modName trgOpt]

                {- Running the core-to-core pipeline. -}
                smplOptGuts <- coreSimplifier $ mkGuts modl tyCons insts cp
                optGuts <- coreOptimizer smplOptGuts

                {- Running the core tidier. -}
                (cgGuts, _) <- coreTidy optGuts

                {- Preparing for Stg generation. -}
                (prepGuts, _) <- corePrepare cgGuts
                {- Stg program. -}
                (stgp, ccs) <- fromCoreToStg prepGuts

                {- Running the stg-to-stg pipeline. -}
                smplStgp <- stgSimplifier prepGuts stgp

                {- C-- code generation. -}
                cmmpStream <- fromStgToCmm prepGuts smplStgp ccs

                {- C-- code optimization and then preparing for code generation. -}
                optCmmStream <- cmmOptimizer prepGuts cmmpStream
                rawCmmStream <- cmmPrepare optCmmStream

                fromCmmTo trgOpt prepGuts rawCmmStream

                {- It eventually creates an interface file (.hi file) according to the value of `intfToMk` -}
                eventuallyMkIface intfToMk prepGuts
                return ()
