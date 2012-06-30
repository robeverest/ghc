-- ----------------------------------------------------------------------------
-- | Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmDecl, LlvmBasicBlock,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmVersion, defaultLlvmVersion, minSupportLlvmVersion,
        maxSupportLlvmVersion,

        LlvmEnv, initLlvmEnv, clearVars, varLookup, varInsert,
        funLookup, funInsert, aliasLookup, aliasInsert, 
        getPointerRegister, reduceAlias, getLlvmVer, 
        setLlvmVer, getLlvmPlatform, getDflags, ghcInternalFunctions,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmFunTy,
        llvmFunSig, llvmStdFunAttrs, llvmFunAlign, llvmInfAlign,
        llvmPtrBits, mkLlvmFunc, tysToParams,

        strCLabel_llvm, genCmmLabelRef, genStringLabelRef

    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Regs

import CLabel
import CgUtils ( activeStgRegs )
import Config
import Constants
import DynFlags
import FastString
import OldCmm
import qualified Outputable as Outp
import Platform
import UniqFM
import Unique
import Data.Maybe
import OrdList

-- ----------------------------------------------------------------------------
-- * Some Data Types
--

type LlvmCmmDecl = GenCmmDecl [LlvmData] (Maybe CmmStatics) (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

-- | Unresolved code.
-- Of the form: (data label, data type, unresolved data)
type LlvmUnresData = (CLabel, Section, LlvmType, [UnresStatic])

-- | Top level LLVM Data (globals and type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

-- | An unresolved Label.
--
-- Labels are unresolved when we haven't yet determined if they are defined in
-- the module we are currently compiling, or an external one.
type UnresLabel  = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

-- ----------------------------------------------------------------------------
-- * Type translations
--

-- | Translate a basic CmmType to an LlvmType.
cmmToLlvmType :: CmmType -> LlvmType
cmmToLlvmType ty | isFloatType ty = widthToLlvmFloat $ typeWidth ty
                 | otherwise      = widthToLlvmInt   $ typeWidth ty

-- | Translate a Cmm Float Width to a LlvmType.
widthToLlvmFloat :: Width -> LlvmType
widthToLlvmFloat W32  = LMFloat
widthToLlvmFloat W64  = LMDouble
widthToLlvmFloat W80  = LMFloat80
widthToLlvmFloat W128 = LMFloat128
widthToLlvmFloat w    = panic $ "widthToLlvmFloat: Bad float size: " ++ show w

-- | Translate a Cmm Bit Width to a LlvmType.
widthToLlvmInt :: Width -> LlvmType
widthToLlvmInt w = LMInt $ widthInBits w

-- | GHC Call Convention for LLVM
llvmGhcCC :: LlvmCallConvention
llvmGhcCC | cGhcUnregisterised == "NO" = CC_Ncc 10
          | otherwise                  = CC_Ccc

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy = LMFunction $ llvmFunSig' (fsLit "a") ExternallyVisible

-- | Llvm Function signature
llvmFunSig :: LlvmEnv -> CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig env lbl link = llvmFunSig' (strCLabel_llvm env lbl) link

llvmFunSig' :: LMString -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig' lbl link
  = let toParams x | isPointer x = (x, [NoAlias, NoCapture])
                   | otherwise   = (x, [])
    in LlvmFunctionDecl lbl link llvmGhcCC LMVoid FixedArgs
                        (map (toParams . getVarType) llvmFunArgs) llvmFunAlign

-- | Create a Haskell function in LLVM.
mkLlvmFunc :: LlvmEnv -> CLabel -> LlvmLinkageType -> LMSection -> LlvmBlocks
           -> LlvmFunction
mkLlvmFunc env lbl link sec blks
  = let funDec = llvmFunSig env lbl link
        funArgs = map (fsLit . getPlainName) llvmFunArgs
    in LlvmFunction funDec funArgs llvmStdFunAttrs sec blks

-- | Alignment to use for functions
llvmFunAlign :: LMAlign
llvmFunAlign = Just wORD_SIZE

-- | Alignment to use for into tables
llvmInfAlign :: LMAlign
llvmInfAlign = Just wORD_SIZE

-- | A Function's arguments
llvmFunArgs :: [LlvmVar]
llvmFunArgs = map lmGlobalRegArg activeStgRegs

-- | Llvm standard fun attributes
llvmStdFunAttrs :: [LlvmFuncAttr]
llvmStdFunAttrs = [NoUnwind]

-- | Convert a list of types to a list of function parameters
-- (each with no parameter attributes)
tysToParams :: [LlvmType] -> [LlvmParameter]
tysToParams = map (\ty -> (ty, []))

-- | Pointer width
llvmPtrBits :: Int
llvmPtrBits = widthInBits $ typeWidth gcWord

-- ----------------------------------------------------------------------------
-- * Llvm Version
--

-- | LLVM Version Number
type LlvmVersion = Int

-- | The LLVM Version we assume if we don't know
defaultLlvmVersion :: LlvmVersion
defaultLlvmVersion = 30

minSupportLlvmVersion :: LlvmVersion
minSupportLlvmVersion = 28

maxSupportLlvmVersion :: LlvmVersion
maxSupportLlvmVersion = 31

-- ----------------------------------------------------------------------------
-- * Environment Handling
--

-- Three maps, one for functions, one for local vars and one for the aliasing 
-- information of local vars.
newtype LlvmEnv = LlvmEnv (LlvmEnvMap, LlvmEnvMap, LlvmAliasMap, LlvmVersion, DynFlags)

type LlvmEnvMap = UniqFM LlvmType

-- A map for the alias information
type LlvmAliasMap = UniqFM (OrdList GlobalReg)

-- | Get initial Llvm environment.
initLlvmEnv :: DynFlags -> LlvmEnv
initLlvmEnv dflags = LlvmEnv (initFuncs, emptyUFM, emptyUFM, defaultLlvmVersion, dflags)
    where initFuncs = listToUFM $ [ (n, LMFunction ty) | (n, ty) <- ghcInternalFunctions ]

-- | Here we pre-initialise some functions that are used internally by GHC
-- so as to make sure they have the most general type in the case that
-- user code also uses these functions but with a different type than GHC
-- internally. (Main offender is treating return type as 'void' instead of
-- 'void *'. Fixes trac #5486.
ghcInternalFunctions :: [(LMString, LlvmFunctionDecl)]
ghcInternalFunctions =
    [ mk "memcpy" i8Ptr [i8Ptr, i8Ptr, llvmWord]
    , mk "memmove" i8Ptr [i8Ptr, i8Ptr, llvmWord]
    , mk "memset" i8Ptr [i8Ptr, llvmWord, llvmWord]
    , mk "newSpark" llvmWord [i8Ptr, i8Ptr]
    ]
  where
    mk n ret args =
        let n' = fsLit n
        in (n', LlvmFunctionDecl n' ExternallyVisible CC_Ccc ret
                                 FixedArgs (tysToParams args) Nothing)

-- | Clear variables from the environment.
clearVars :: LlvmEnv -> LlvmEnv
clearVars (LlvmEnv (e1, _, _, n, p)) = {-# SCC "llvm_env_clear" #-}
    LlvmEnv (e1, emptyUFM, emptyUFM, n, p)

-- | Insert local variables into the environment.
varInsert :: Uniquable key => key -> LlvmType -> LlvmEnv -> LlvmEnv
varInsert s t (LlvmEnv (e1, e2, e3, n, p)) = {-# SCC "llvm_env_vinsert" #-}
    LlvmEnv (e1, addToUFM e2 s t, e3, n, p)

-- | Insert functions into the environment.
funInsert :: Uniquable key => key -> LlvmType -> LlvmEnv -> LlvmEnv
funInsert s t (LlvmEnv (e1, e2, e3, n, p)) = {-# SCC "llvm_env_finsert" #-}
    LlvmEnv (addToUFM e1 s t, e2, e3, n, p)

-- | Lookup local variables in the environment.
varLookup :: Uniquable key => key -> LlvmEnv -> Maybe LlvmType
varLookup s (LlvmEnv (_, e2, _, _, _)) = {-# SCC "llvm_env_vlookup" #-}
    lookupUFM e2 s

-- | Lookup functions in the environment.
funLookup :: Uniquable key => key -> LlvmEnv -> Maybe LlvmType
funLookup s (LlvmEnv (e1, _, _, _, _)) = {-# SCC "llvm_env_flookup" #-}
    lookupUFM e1 s

-- | Get the "pointer" register.
getPointerRegister :: OrdList GlobalReg -> GlobalReg
getPointerRegister gs = fromMaybe Hp $ foldlOL getPointerRegister' Nothing gs 
                         -- If a local variable is not derived from any STG pointer register
                         -- then it must be of the form t = I32[X] or similiar. These pointers 
                         -- can be classified as "heap"
  where
    getPointerRegister' (Just Hp) Sp = panic "Pointer is derived from both heap and stack pointer"
    getPointerRegister' (Just Sp) Hp = panic "Pointer is derived from both heap and stack pointer"
    getPointerRegister' _ Hp = Just Hp
    getPointerRegister' _ Sp = Just Sp
    getPointerRegister' _ BaseReg = Just BaseReg
    getPointerRegister' Nothing (VanillaReg i p) = Just $ VanillaReg i p
    getPointerRegister' a _ = a
    
-- | Reduce the alias information for each local variable down to just the "pointer" register
reduceAlias :: LlvmEnv -> LlvmEnv
reduceAlias (LlvmEnv (e1, e2, e3, n, p)) = let
  e3' = (unitOL.getPointerRegister) `fmap` e3 
  in LlvmEnv (e1, e2, e3', n, p)
     
-- | Lookup aliasing information of local variables in the environment.
aliasLookup :: Uniquable key => key -> LlvmEnv -> Maybe (OrdList GlobalReg)
aliasLookup s (LlvmEnv (_, _, e3, _, _)) = lookupUFM e3 s

-- | Insert aliasing information of local variables into the environment.
aliasInsert :: Uniquable key => key -> OrdList GlobalReg -> LlvmEnv -> LlvmEnv
aliasInsert s gr (LlvmEnv (e1, e2, e3, n, p)) = LlvmEnv (e1, e2, addToUFM e3 s gr, n, p)

-- | Get the LLVM version we are generating code for
getLlvmVer :: LlvmEnv -> LlvmVersion
getLlvmVer (LlvmEnv (_, _, _, n, _)) = n

-- | Set the LLVM version we are generating code for
setLlvmVer :: LlvmVersion -> LlvmEnv -> LlvmEnv
setLlvmVer n (LlvmEnv (e1, e2, e3, _, p)) = LlvmEnv (e1, e2, e3, n, p)

-- | Get the platform we are generating code for
getLlvmPlatform :: LlvmEnv -> Platform
getLlvmPlatform (LlvmEnv (_, _, _, _, d)) = targetPlatform d

-- | Get the DynFlags for this compilation pass
getDflags :: LlvmEnv -> DynFlags
getDflags (LlvmEnv (_, _, _, _, d)) = d

-- ----------------------------------------------------------------------------
-- * Label handling
--

-- | Pretty print a 'CLabel'.
strCLabel_llvm :: LlvmEnv -> CLabel -> LMString
strCLabel_llvm env l = {-# SCC "llvm_strCLabel" #-}
    (fsLit . toString . pprCLabel (getLlvmPlatform env)) l
    where dflags = getDflags env
          style = Outp.mkCodeStyle Outp.CStyle
          toString doc = Outp.renderWithStyle dflags doc style

-- | Create an external definition for a 'CLabel' defined in another module.
genCmmLabelRef :: LlvmEnv -> CLabel -> LMGlobal
genCmmLabelRef env = genStringLabelRef . strCLabel_llvm env

-- | As above ('genCmmLabelRef') but taking a 'LMString', not 'CLabel'.
genStringLabelRef :: LMString -> LMGlobal
genStringLabelRef cl
  = let ty = LMPointer $ LMArray 0 llvmWord
    in (LMGlobalVar cl ty External Nothing Nothing False, Nothing)

-- ----------------------------------------------------------------------------
-- * Misc
--

-- | Error function
panic :: String -> a
panic s = Outp.panic $ "LlvmCodeGen.Base." ++ s

