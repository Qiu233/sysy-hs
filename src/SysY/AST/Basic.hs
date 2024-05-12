{-# LANGUAGE TemplateHaskell #-}
module SysY.AST.Basic where
import qualified Language.Haskell.TH.Syntax

type Ident = String

data Decl =
    DeclConst ConstDecl |
    DeclVar VarDecl
    deriving (Eq, Show)

data ConstDecl = ConstDecl BType [ConstDef]
    deriving (Eq, Show)

data BType = BInt | BFloat
    deriving (Eq, Show)

data ConstDef = ConstDef Ident [Exp] ConstInitVal
    deriving (Eq, Show)

data ConstInitVal =
    ConstInitExp Exp |
    ConstInitArray [ConstInitVal]
    deriving (Eq, Show)

data VarDecl = VarDecl BType [VarDef]
    deriving (Eq, Show)

data VarDef =
    VarDefUninit Ident [Exp] |
    VarDefInit Ident [Exp] InitVal
    deriving (Eq, Show)

data InitVal =
    InitValExp Exp |
    InitValArray [InitVal]
    deriving (Eq, Show)

data FuncDef = FuncDef FuncType Ident [FuncFParam] Block
    deriving (Eq, Show)

data FuncType = FVoid | FInt | FFloat
    deriving (Eq, Show)

data FuncFParam = FuncFParam BType Ident Int [Exp]
    deriving (Eq, Show)

newtype Block = Block [BlockItem]
    deriving (Eq, Show)

data BlockItem = BlockItemDecl Decl | BlockItemStmt Stmt
    deriving (Eq, Show)

data Stmt =
    StmtLVal LVal Exp |
    StmtExp (Maybe Exp) |
    StmtBlock Block |
    StmtIf Exp Stmt (Maybe Stmt) |
    StmtWhile Exp Stmt |
    StmtBreak |
    StmtContinue |
    StmtReturn (Maybe Exp)
    deriving (Eq, Show)

data LVal = LVal Ident [Exp]
    deriving (Eq, Show)

data Number =
    IntConst Integer |
    FloatConst Float
    deriving (Eq, Show)

data Exp =
    ExpLVal LVal |
    ExpNum Number |
    ExpOpUnary Optr Exp |
    ExpOpBinary Optr Exp Exp |
    ExpCall Ident [Exp]
    deriving (Eq, Show)

data Optr =
    Plus | Minus | Flip |
    Mul | Div | Mod |
    Lt | Gt | Le | Ge |
    Eq | Ne |
    LAnd | LOr
    deriving (Eq, Show)

data TopLevel = TLDecl Decl | TLFun FuncDef
    deriving (Eq, Show)

newtype CompUnit = CompUnit [TopLevel]
    deriving (Eq, Show)

astTypes :: [Language.Haskell.TH.Syntax.Name]
astTypes = [''Decl, ''ConstDecl, ''BType, ''ConstDef, ''ConstInitVal, ''VarDecl, 
            ''VarDef, ''InitVal, ''FuncDef, ''FuncType, ''FuncFParam, ''Block,
            ''BlockItem, ''Stmt, ''LVal, ''Number, ''Exp, ''Optr, ''TopLevel, ''CompUnit]
