{-# LANGUAGE TemplateHaskell #-}
module SysY.AST.Basic where

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

data ConstInitVal =
    ConstInitExp TypedExp |
    ConstInitArray [ConstInitVal]
    deriving (Eq, Show)

data VarDecl = VarDecl BType [VarDef]
    deriving (Eq, Show)

data VarDef =
    VarDefUninit Ident [TypedExp] |
    VarDefInit Ident [TypedExp] InitVal
    deriving (Eq, Show)

data InitVal =
    InitValExp TypedExp |
    InitValArray [InitVal]
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
    -- when type inference fails or the type 
    -- doesn't matter in terms of type check
    = TermAny
    | TermBType BType
    | TermArray BType [Integer]
    -- the special type `type[][a][b]...`
    | TermUArray BType [Integer]
    -- Function has no type in SysY
    --- | TermFunc SysYType [SysYType]
    deriving (Eq, Show)
    
type TypedExp = (Maybe TermType, Exp)

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

-- astTypes :: [Language.Haskell.TH.Syntax.Name]
-- astTypes = [''Decl, ''ConstDecl, ''BType, ''ConstDef, ''ConstInitVal, ''VarDecl, 
--             ''VarDef, ''InitVal, ''FuncDef, ''FuncType, ''FuncFParam, ''Block,
--             ''BlockItem, ''Stmt, ''LVal, ''Number, ''Exp, ''Optr, ''TopLevel, ''CompUnit]
