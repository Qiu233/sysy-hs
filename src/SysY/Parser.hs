{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module SysY.Parser where
import Text.Parsec
import Text.Parsec.String
import SysY.AST
import qualified Text.Parsec.Token as P
import Data.Functor (($>), (<&>))
import Control.Monad (void)
import Text.Parsec.Expr
import GHC.Float (double2Float)

parse_CompUnit :: Parser CompUnit
parse_CompUnit = CompUnit <$> many1 ((try parse_Decl <&> TLDecl) <|> (try parse_FuncDef <&> TLFun))

parse_Decl :: Parser Decl
parse_Decl = (try parse_ConstDecl <&> DeclConst) <|> (parse_VarDecl <&> DeclVar)

parse_ConstDecl :: Parser ConstDecl
parse_ConstDecl = do
    reserved "const"
    ConstDecl <$> parse_BType <*> commaSep1 parse_ConstDef

parse_BType :: Parser BType
parse_BType =
    (reserved "int" $> BInt) <|>
    (reserved "float" $> BFloat)

parse_ConstDef :: Parser ConstDef
parse_ConstDef = do
    name <- parse_Ident
    dimensions <- parse_arr_indexers
    parse_assign_eq_op
    init_ <- parse_ConstInitVal
    pure $ ConstDef name dimensions init_

parse_ConstInitVal :: Parser ConstInitVal
parse_ConstInitVal = do
    (try parse_Exp <&> ConstInitExp) <|> (braces (do commaSep parse_ConstInitVal) <&> ConstInitArray)

parse_VarDecl :: Parser VarDecl
parse_VarDecl = do
    type_ <- parse_BType
    defs <- commaSep1 parse_VarDef
    _ <- semi
    pure $ VarDecl type_ defs

parse_VarDef :: Parser VarDef
parse_VarDef = do
    name <- parse_Ident
    indexers <- parse_arr_indexers
    (try parse_assign_eq_op >> (parse_InitVal >>= \init_ -> pure $ VarDefInit name indexers init_))
        <|> pure (VarDefUninit name indexers)

parse_InitVal :: Parser InitVal
parse_InitVal = do
    (try parse_Exp <&> InitValExp) <|>
        (braces (commaSep parse_InitVal) <&> InitValArray)

parse_FuncDef :: Parser FuncDef
parse_FuncDef = do
    type_ <- parse_FuncType
    name <- parse_Ident
    params <- parens parse_FuncFParams
    block <- parse_Block
    pure $ FuncDef type_ name params block

parse_FuncType :: Parser FuncType
parse_FuncType =
    (reserved "int" $> FInt) <|>
    (reserved "float" $> FFloat) <|>
    (reserved "void" $> FVoid)

parse_FuncFParams :: Parser [FuncFParam]
parse_FuncFParams = commaSep parse_FuncFParam

parse_FuncFParam :: Parser FuncFParam
parse_FuncFParam = do
    type_ <- parse_BType
    name <- parse_Ident
    try (do
        brackets $ pure ()
        (try parse_arr_indexers >>= \indexers -> pure $ FuncFParam type_ name (1 + length indexers) indexers)
            <|> pure (FuncFParam type_ name 1 []))
        <|> pure (FuncFParam type_ name 0 [])

parse_Block :: Parser Block
parse_Block = Block <$> braces (many parse_BlockItem)

parse_BlockItem :: Parser BlockItem
parse_BlockItem = (try parse_Decl <&> BlockItemDecl) <|> (try parse_Stmt <&> BlockItemStmt)

parse_Stmt_LVal :: Parser Stmt
parse_Stmt_LVal =
    parse_LVal >>= \lval ->
        parse_assign_eq_op >> parse_Exp >>= \exp_ ->
            semi >> pure (StmtLVal lval exp_)

parse_Stmt_Exp :: Parser Stmt
parse_Stmt_Exp = StmtExp <$> (optionMaybe parse_Exp <* semi)

parse_Stmt_If :: Parser Stmt
parse_Stmt_If = do
    reserved "if"
    cond <- parens parse_Cond
    body_true <- parse_Stmt
    body_false <- optionMaybe $ try $ do
        reserved "else"
        parse_Stmt
    pure $ StmtIf cond body_true body_false

parse_Stmt_While :: Parser Stmt
parse_Stmt_While = do
    reserved "while"
    cond <- parens parse_Cond
    body <- parse_Stmt
    pure $ StmtWhile cond body

parse_Stmt_Return :: Parser Stmt
parse_Stmt_Return = do
    reserved "return"
    ret_val <- optionMaybe $ try parse_Exp
    _ <- semi
    pure $ StmtReturn ret_val

parse_Stmt :: Parser Stmt
parse_Stmt = do
    try parse_Block <&> StmtBlock
    <|> try parse_Stmt_If
    <|> try parse_Stmt_While
    <|> try (reserved "break" *> semi $> StmtBreak)
    <|> try (reserved "continue" *> semi $> StmtContinue)
    <|> try parse_Stmt_Return
    <|> try parse_Stmt_LVal
    <|> parse_Stmt_Exp

parse_Exp :: Parser Exp
parse_Exp = buildExpressionParser (take 3 table) parse_PrimaryExp

parse_Cond :: Parser Exp
parse_Cond = buildExpressionParser table parse_PrimaryExp

parse_LVal :: Parser LVal
parse_LVal = do
    name <- parse_Ident
    indexers <- parse_arr_indexers
    pure $ LVal name indexers

parse_PrimaryExp_parens :: Parser Exp
parse_PrimaryExp_parens = parens parse_Exp

parse_PrimaryExp :: Parser Exp
parse_PrimaryExp = do
    try parse_PrimaryExp_parens
    <|> try parse_UnaryExp_Call
    <|> (try parse_LVal <&> ExpLVal)
    <|> (parse_Number <&> ExpNum)

parse_Number :: Parser Number
parse_Number = (try parse_FloatConst <&> FloatConst) <|> (parse_IntConst <&> IntConst)

parse_UnaryExp_Call :: Parser Exp
parse_UnaryExp_Call = do
    name <- parse_Ident
    rparams <- parens parse_FuncRParams
    pure $ ExpCall name rparams

parse_FuncRParams :: Parser [Exp]
parse_FuncRParams = commaSep parse_Exp

sysy_lang :: P.LanguageDef st
sysy_lang = P.LanguageDef {
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False,
        P.identStart = letter <|> char '_',
        P.identLetter = alphaNum <|> char '_',
        P.opStart = oneOf "",
        P.opLetter = oneOf "",
        P.reservedNames = ["void", "int", "float", "const", "if", "else", "while", "break", "return"],
        P.reservedOpNames = ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||"], -- assignment
        P.caseSensitive = True
    }

parse_assign_eq_op :: Parser ()
parse_assign_eq_op = void $ P.lexeme lexer $ char '='

parse_arr_indexers :: Parser [Exp]
parse_arr_indexers = many $ brackets parse_Exp

parse_Integer_decimal :: Parser Integer
parse_Integer_decimal = P.lexeme lexer $ P.decimal lexer

parse_Integer_hex :: Parser Integer
parse_Integer_hex = P.lexeme lexer $ do
    body <- char '0' >> oneOf "xX" >> many1 hexDigit
    pure $ read $ "0x" ++ body

parse_Integer_octal :: Parser Integer
parse_Integer_octal = P.lexeme lexer $ do
    body <- char '0' >> many1 octDigit
    pure $ read $ "0o" ++ body

parse_IntConst :: Parser Integer
parse_IntConst = try parse_Integer_hex <|> try parse_Integer_octal <|> parse_Integer_decimal

parse_FloatConst :: Parser Float
parse_FloatConst = double2Float <$> P.float lexer -- TODO: parse float instead of converting

lexer = P.makeTokenParser sysy_lang

parse_Ident = P.identifier lexer
brackets = P.brackets lexer
parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
comma = P.comma lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
semi = P.semi lexer
reservedOp = P.reservedOp lexer

table   = [[prefix "+" id, prefix "-" (ExpOpUnary Minus), prefix "!" (ExpOpUnary Flip) ]
        , [binary "*" (ExpOpBinary Mul) AssocLeft, binary "/" (ExpOpBinary Div) AssocLeft, binary "%" (ExpOpBinary Mod) AssocLeft ]
        , [binary "+" (ExpOpBinary Plus) AssocLeft, binary "-" (ExpOpBinary Minus) AssocLeft ]
        , [binary "<" (ExpOpBinary Lt) AssocLeft, binary ">" (ExpOpBinary Gt) AssocLeft, binary "<=" (ExpOpBinary Le) AssocLeft, binary ">=" (ExpOpBinary Ge) AssocLeft ]
        , [binary "==" (ExpOpBinary Eq) AssocLeft, binary "!=" (ExpOpBinary Ne) AssocLeft ]
        , [binary "&&" (ExpOpBinary LAnd) AssocLeft ]
        , [binary "||" (ExpOpBinary LOr) AssocLeft ]
        ]

binary name fun = Infix (do { reservedOp name; return fun })
prefix  name fun = Prefix (do { reservedOp name; return fun })
postfix name fun = Postfix (do { reservedOp name; return fun })