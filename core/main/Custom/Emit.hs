{-# LANGUAGE OverloadedStrings #-}

module Custom.Emit where

import LLVM.General.Module
import LLVM.General.Context

import LLVM.General.Target
import LLVM.General.ExecutionEngine
import Foreign.Ptr
import Foreign.C.Types

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
--import qualified LLVM.General.AST.Integer as I
--import qualified LLVM.General.AST.Float as F
--import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP

import Data.Word
import Data.Int
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Custom.Codegen
import qualified Custom.Syntax as S

mainName :: String
mainName = "__main__"

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (int64, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define int64 name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca int64
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external int64 name fnargs []
  where fnargs = toSig args

codegenTop exp = do
  define int64 mainName [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = icmp IP.ULT a b
  --uitofp int64 test

binops = Map.fromList [
      ("+", iadd)
    , ("-", isub)
    , ("*", imul)
    , ("/", idiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
--cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Constant n) = return $ cons $ C.Int 64 n
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

type MainFunction = IO Int64
foreign import ccall "dynamic" 
  haskFun :: FunPtr MainFunction -> MainFunction

run :: FunPtr a -> MainFunction
run fn = haskFun (castFunPtr fn :: FunPtr MainFunction)

jit :: Context -> (MCJIT -> IO a) -> IO a
jit c = withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    liftError $ withDefaultTargetMachine $ \target -> do
      liftError $ writeAssemblyToFile target "/Users/janmachacek/foo.S" m
      liftError $ writeObjectToFile target "/Users/janmachacek/foo.o" m
      llstr <- moduleString m
      putStrLn llstr

    jit context $ \executionEngine -> do
      withModuleInEngine executionEngine m $ \em -> do
        maybeFun <- getFunction em (AST.Name mainName)
        case maybeFun of
          Just fun -> do
            val <- run fun
            putStrLn $ "******** :) " ++ (show val)
          Nothing ->
            putStrLn ":("

        return ()

    --withDefaultTargetMachine $ \machine -> moduleAssembly machine m
    --astr  <- moduleAssembly 
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
