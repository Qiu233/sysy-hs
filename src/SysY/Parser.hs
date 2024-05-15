{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TupleSections #-}
module SysY.Parser where
import Text.Parsec
import Text.Parsec.String
import SysY.AST
import qualified Text.Parsec.Token as P
import Data.Functor (($>), (<&>))
import Control.Monad (void)
import Text.Parsec.Expr
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Numbers.FloatingHex as FH

parse_CompUnit :: Parser CompUnit
parse_CompUnit = CompUnit <$> (P.whiteSpace lexer *> many1 ((try (parse_Decl <* notFollowedBy (P.lexeme lexer (char '('))) <&> TLDecl) <|> (parse_FuncDef <&> TLFun)) <* eof)

parse_Decl :: Parser Decl
parse_Decl = (try parse_ConstDecl <&> DeclConst) <|> (parse_VarDecl <&> DeclVar)

parse_ConstDecl :: Parser ConstDecl
parse_ConstDecl = do
    reserved "const"
    ConstDecl <$> parse_BType <*> (commaSep1 parse_ConstDef <* semi)

parse_BType :: Parser BType
parse_BType =
    (reserved "int" $> BInt) <|>
    (reserved "float" $> BFloat)

parse_ConstDef :: Parser ConstDef
parse_ConstDef = do
    name <- parse_Ident
    dimensions <- parse_arr_indexers
    parse_assign_eq_op
    init_ <- parse_InitVal
    pure $ ConstDef name dimensions init_

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

parse_InitVal :: Parser ConstInitVal
parse_InitVal = do
    (try parse_Exp <&> (ConstInfo Nothing, ) . InitValExp) <|>
        (braces (commaSep parse_InitVal) <&> (ConstInfo Nothing, ) . InitValArray)

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

parse_Exp :: Parser TypedExp
parse_Exp = buildExpressionParser (take 3 table) parse_PrimaryExp

parse_Cond :: Parser TypedExp
parse_Cond = buildExpressionParser table parse_PrimaryExp

parse_LVal :: Parser LVal
parse_LVal = do
    name <- parse_Ident
    indexers <- parse_arr_indexers
    pure $ LVal name indexers

parse_PrimaryExp_parens :: Parser TypedExp
parse_PrimaryExp_parens = parens parse_Exp

parse_PrimaryExp :: Parser TypedExp
parse_PrimaryExp = do
    try parse_PrimaryExp_parens
    <|> try parse_UnaryExp_Call
    <|> (try parse_LVal <&> RawExp . ExpLVal)
    <|> (parse_Number <&> RawExp . ExpNum)

parse_Number :: Parser Number -- TODO: improve error message
parse_Number = (try parse_FloatConst <&> FloatConst) <|> (parse_IntConst <&> IntConst)

parse_UnaryExp_Call :: Parser TypedExp
parse_UnaryExp_Call = do
    name <- parse_Ident
    rparams <- parens parse_FuncRParams
    pure $ RawExp $ ExpCall name rparams

parse_FuncRParams :: Parser [TypedExp]
parse_FuncRParams = commaSep parse_Exp

sysy_lang :: P.LanguageDef st
sysy_lang = P.LanguageDef {
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False,
        P.identStart = letter <|> char '_',
        P.identLetter = alphaNum <|> char '_',
        P.opStart = oneOf "<>=&|!",
        P.opLetter = oneOf "=&|",
        P.reservedNames = ["void", "int", "float", "const", "if", "else", "while", "break", "return"],
        P.reservedOpNames = ["+", "-", "*", "/", "%", "="],
        P.caseSensitive = True
    }

parse_assign_eq_op :: Parser ()
parse_assign_eq_op = void $ P.lexeme lexer $ char '='

parse_arr_indexers :: Parser [TypedExp]
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
parse_FloatConst = P.lexeme lexer (try parse_Float_Hex <|> parse_Float_Decimal)

parse_Float_Hex1 :: Parser Float
parse_Float_Hex1 = do
    _ <- char '0' >> oneOf "xX"
    frac <- parse_Float_Fractional_Constant_Hex
    exponent_ <- parse_Float_Exponent_Bin
    pure $ fromJust $ FH.readHFloat $ "0x" ++ frac ++ exponent_

parse_Float_Hex2 :: Parser Float
parse_Float_Hex2 = do
    _ <- char '0' >> oneOf "xX"
    digits <- parse_Float_DigitSeq_Hex
    exponent_ <- parse_Float_Exponent_Bin
    pure $ fromJust $ FH.readHFloat $ "0x" ++ digits ++ exponent_

parse_Float_Hex :: Parser Float
parse_Float_Hex = do
    try parse_Float_Hex1 <|> parse_Float_Hex2

parse_Float_Exponent_Bin :: Parser String
parse_Float_Exponent_Bin = do
    _ <- oneOf "pP"
    sign <- optionMaybe $ oneOf "+-"
    digits <- parse_Float_DigitSeq
    pure $ "p" ++ (fromMaybe '+' sign : digits)

parse_Float_Fractional_Constant1_Hex :: Parser String
parse_Float_Fractional_Constant1_Hex = do
    pre <- optionMaybe parse_Float_DigitSeq_Hex
    _ <- P.dot lexer
    post <- parse_Float_DigitSeq_Hex
    pure $ fromMaybe "0" pre ++ ('.' : post)

parse_Float_Fractional_Constant2_Hex :: Parser String
parse_Float_Fractional_Constant2_Hex = do
    pre <- parse_Float_DigitSeq_Hex
    _ <- P.dot lexer
    pure $ pre ++ ".0"

parse_Float_Fractional_Constant_Hex :: Parser String
parse_Float_Fractional_Constant_Hex =
    try parse_Float_Fractional_Constant1_Hex <|> parse_Float_Fractional_Constant2_Hex

parse_Float_Decimal1 :: Parser Float
parse_Float_Decimal1 = do
    frac <- parse_Float_Fractional_Constant
    exponent_ <- optionMaybe parse_Float_Exponent
    pure $ read $ frac ++ fromMaybe "" exponent_

parse_Float_Decimal2 :: Parser Float
parse_Float_Decimal2 = do
    digits <- parse_Float_DigitSeq
    exponent_ <- parse_Float_Exponent
    pure $ read $ digits ++ exponent_

parse_Float_Decimal :: Parser Float
parse_Float_Decimal = do
    try parse_Float_Decimal1 <|> parse_Float_Decimal2

parse_Float_Exponent :: Parser String
parse_Float_Exponent = do
    _ <- oneOf "eE"
    sign <- optionMaybe $ oneOf "+-"
    digits <- parse_Float_DigitSeq
    pure $ "e" ++ (fromMaybe '+' sign : digits)

parse_Float_Fractional_Constant1 :: Parser String
parse_Float_Fractional_Constant1 = do
    pre <- optionMaybe parse_Float_DigitSeq
    _ <- P.dot lexer
    post <- parse_Float_DigitSeq
    pure $ fromMaybe "0" pre ++ ('.' : post)

parse_Float_Fractional_Constant2 :: Parser String
parse_Float_Fractional_Constant2 = do
    pre <- parse_Float_DigitSeq
    _ <- P.dot lexer
    pure $ pre ++ ".0"

parse_Float_Fractional_Constant :: Parser String
parse_Float_Fractional_Constant =
    try parse_Float_Fractional_Constant1 <|> parse_Float_Fractional_Constant2

parse_Float_DigitSeq :: Parser String
parse_Float_DigitSeq = many1 digit

parse_Float_DigitSeq_Hex :: Parser String
parse_Float_DigitSeq_Hex = many1 hexDigit

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

op_u o t = RawExp $ ExpOpUnary o t
op_b o t1 t2 = RawExp $ ExpOpBinary o t1 t2

table   = [[prefix "+" id, prefix "-" (op_u Minus), prefix "!" (op_u Flip) ]
        , [binary "*" (op_b Mul) AssocLeft, binary "/" (op_b Div) AssocLeft, binary "%" (op_b Mod) AssocLeft ]
        , [binary "+" (op_b Plus) AssocLeft, binary "-" (op_b Minus) AssocLeft ]
        , [binary "<" (op_b Lt) AssocLeft, binary ">" (op_b Gt) AssocLeft, binary "<=" (op_b Le) AssocLeft, binary ">=" (op_b Ge) AssocLeft ]
        , [binary "==" (op_b Eq) AssocLeft, binary "!=" (op_b Ne) AssocLeft ]
        , [binary "&&" (op_b LAnd) AssocLeft ]
        , [binary "||" (op_b LOr) AssocLeft ]
        ]

binary name fun = Infix (do { reservedOp name; return fun })
prefix  name fun = Prefix (do { reservedOp name; return fun })
postfix name fun = Postfix (do { reservedOp name; return fun })