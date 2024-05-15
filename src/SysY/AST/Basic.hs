{-# LANGUAGE TemplateHaskell #-}
module SysY.AST.Basic where
import Data.Maybe (isJust)

type Ident = String

data Decl =
    DeclConst ConstDecl |
    DeclVar VarDecl
    deriving (Eq, Show)

-- TODO: can we flatten the defs?
data ConstDecl = ConstDecl BType [ConstDef]
    deriving (Eq, Show)

data BType = BInt | BFloat
    deriving (Eq, Show)

data ConstDef = ConstDef Ident [TypedExp] ConstInitVal
    deriving (Eq, Show)

data VarDecl = VarDecl BType [VarDef]
    deriving (Eq, Show)

data VarDef =
    VarDefUninit Ident [TypedExp] |
    VarDefInit Ident [TypedExp] ConstInitVal
    deriving (Eq, Show)

data InitVal =
    InitValExp TypedExp |
    InitValArray [ConstInitVal]
    deriving (Eq, Show)

data FuncDef = FuncDef FuncType Ident [FuncFParam] Block
    deriving (Eq, Show)

data FuncType = FVoid | FInt | FFloat
    deriving (Eq, Show)

data FuncFParam = FuncFParam BType Ident Int [TypedExp]
    deriving (Eq, Show)

newtype Block = Block [BlockItem]
    deriving (Eq, Show)

data BlockItem = BlockItemDecl Decl | BlockItemStmt Stmt
    deriving (Eq, Show)

data Stmt =
    StmtLVal LVal TypedExp |
    StmtExp (Maybe TypedExp) |
    StmtBlock Block |
    StmtIf TypedExp Stmt (Maybe Stmt) |
    StmtWhile TypedExp Stmt |
    StmtBreak |
    StmtContinue |
    StmtReturn (Maybe TypedExp)
    deriving (Eq, Show)

data LVal = LVal Ident [TypedExp]
    deriving (Eq, Show)

data Number =
    IntConst Integer |
    FloatConst Float
    deriving (Eq, Show)


data TermType
    = TermAny
    | TermVoid
    | TermBType BType
    | TermArray BType [Maybe Integer]
    deriving (Eq, Show)

-- type TypedExp = (Maybe TermType, Exp)
data TypedExp
    = RawExp Exp
    | TypedExp TermType Exp
    | ConvExp TermType TypedExp
    deriving (Eq, Show)

data ConstVal
    = ConstValInt Integer
    | ConstValFloat Float
    | ConstValArray [ConstVal] -- interpresentation is dependent on type hint
    deriving (Eq, Show)

newtype ConstInfo = ConstInfo (Maybe ConstVal)
    deriving (Eq, Show)
type ConstInitVal = (ConstInfo, InitVal)

constVal :: ConstInitVal -> Maybe ConstVal
constVal (ConstInfo v, _) = v

hasConst :: ConstInitVal -> Bool
hasConst = isJust . constVal

data Exp =
    ExpLVal LVal |
    ExpNum Number |
    ExpOpUnary Optr TypedExp |
    ExpOpBinary Optr TypedExp TypedExp |
    ExpCall Ident [TypedExp]
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

