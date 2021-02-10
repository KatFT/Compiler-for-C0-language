{
module Parser where
import Lexer
}

%name parser                             -- nome da função de parsing
%tokentype { Token }
%error { parseError }

%token                                   -- definição dos valores de terminais

num         { NUM $$ }
id          { ID $$ }
'+'         { PLUS }
'-'         { MINUS }
'*'         { MULT }
'%'         { MOD }
'/'         { DIV }
'('         { LPAREN }
')'         { RPAREN }
'{'         { LBRACE }
'}'         { RBRACE }
';'         { SEMICOLON }
','         { COMMA }
'='         { ASSIGN }
'>'         { GREATER }
'<'         { LESS }
'>='        { GREATERorEQUAL }
'<='        { LESSorEQUAL }
'!='        { DIFF }
'=='        { EQUAL }
'!'         { NOT }
'&&'        { AND }
'||'        { OR }
'true'      { TRUE }
'false'     { FALSE }
'bool'      { BOOL }
'int'       { INT }
if          { IF }
else        { ELSE }
return      { RETURN }
while       { WHILE }
for         { FOR }
break       { BREAK }
continue    { CONTINUE }
'print_int' { PRINTint }

-- precedencia mais baixa
%nonassoc '<' '>' '==' '!=' '<=' '>=' '&&' '||'
%left '+' '-'
%left '*' '/'
%left '%'
%nonassoc OUTERTHEN
%nonassoc else
-- precedencia mais alta

%%

-- Functions
nextFun : Fun                                                { [$1] }
        | Fun nextFun                                        { $1 : $2 }

Fun : Tp id '(' Arg ')' '{' nextStmt '}'                     { FunDef $2 $1 $4 $7 }
    | Tp id '(' ')' '{' nextStmt '}'                         { FunDef $2 $1 [] $6 }

-- Functions arguments
Arg : Tp id                                                  { [($2, $1)] }
    | Tp id ',' Arg                                          { ($2, $1) : $4 }
  
-- Statements
Stmt : Simple ';'                                            { $1 }
     | Declr ';'                                             { $1 }
     | Increment ';'                                         { $1 }
     | whileStmt                                             { $1 }
     | forStmt                                               { $1 }
     | '{' nextStmt '}'                                      { Block $2 }
     | return Exp ';'                                        { Return $2 }
     | 'print_int' '(' Exp ')' ';'                           { PrintINT $3 }
     | if '(' Exp ')' Stmt else Stmt                         { IfElse $3 $5 $7 }
     | if '(' Exp ')' Stmt %prec OUTERTHEN                   { If $3 $5 }

nextStmt : Stmt                                              { [$1] }
         | Stmt nextStmt                                     { $1 : $2 }

StmtBC : Simple ';'                                          { $1 }
       | Declr ';'                                           { $1 }
       | Increment ';'                                       { $1 }
       | if '(' Exp ')' StmtBC else StmtBC                   { IfElse $3 $5 $7 } 
       | if '(' Exp ')' StmtBC %prec OUTERTHEN               { If $3 $5 }
       | whileStmt                                           { $1 }
       | '{' '}'                                             { Skip }
       | '{' nextStmtBC '}'                                  { Block $2 }
       | return Exp ';'                                      { Return $2 }
       | 'print_int' '(' Exp ')' ';'                         { PrintINT $3 }
       | break ';'                                           { Break }
       | continue ';'                                        { Continue }
       | forStmt                                             { $1 }

nextStmtBC : StmtBC                                          { [$1] }
           | StmtBC nextStmtBC                               { $1 : $2 }

whileStmt : while '(' Exp ')' StmtBC                         { While $3 $5 }


forStmt : for '(' Simple ';' Exp ';' Increment ')' StmtBC    { For $3 $5 $7 $9 }
        | for '(' ';' Exp ';' ')' StmtBC                     { For Skip $4 Skip $7 }
        | for '(' Simple ';' Exp ';' ')' StmtBC              { For $3 $5 Skip $8 }
        | for '(' ';' Exp ';' Increment ')' StmtBC           { For Skip $4 $6 $8 }
 
-- Assignments and declarations
Simple : id '=' Exp                                          { Assign $1 $3 }

Declr : Tp id                                                { Decl ($2, $1) }

Increment : id '+' '+'                                       { Incr $1 }
          | id '-' '-'                                       { Decr $1 }

-- Types allowed
Tp : 'int'                                                   { TyInt }
   | 'bool'                                                  { TyBool }

-- elements of the function call
ArgFunCall : Exp                                             { [$1] }
           | Exp ',' ArgFunCall                              { $1 : $3 }

-- terms
Term : num                                                   { Num $1 }
     | id                                                    { Var $1 }
     | '(' Exp ')'                                           { $2 }
     | boolean                                               { $1 }
     | '!' '(' Exp ')'                                       { Not $3 }

-- Expressions
Exp  : Term                                                  { $1 }
     | Exp '+' Exp                                           { OpBin Plus $1 $3 }
     | Exp '-' Exp                                           { OpBin Minus $1 $3 }
     | Exp '*' Exp                                           { OpBin Times $1 $3 }
     | Exp '/' Exp                                           { OpBin Div $1 $3 }
     | Exp '%' Exp                                           { OpBin Mod $1 $3 }
     | Exp '||' Exp                                          { OpRel Or $1 $3 }
     | Exp '&&' Exp                                          { OpRel And $1 $3 }
     | Exp '>' Exp                                           { OpRel Gt $1 $3 }
     | Exp '<' Exp                                           { OpRel Lt $1 $3 }
     | Exp '>=' Exp                                          { OpRel Gteq $1 $3 }
     | Exp '<=' Exp                                          { OpRel Lteq $1 $3 }
     | Exp '==' Exp                                          { OpRel Eq $1 $3 }
     | Exp '!=' Exp                                          { OpRel Diff $1 $3 }
     | id '(' ')'                                            { FunCall $1 [] }
     | id '(' ArgFunCall ')'                                 { FunCall $1 $3 }

-- Boolean values
boolean : 'true'                                             { BoolVal ValTrue }
        | 'false'                                            { BoolVal ValFalse }

{

type Ident = String

data Type = TyInt
          | TyBool
          deriving (Show, Eq)

data Boolean = ValTrue
             | ValFalse
             deriving (Show, Eq)

-- statements
data Stmt = Assign Ident Exp           -- ident = exp
          | IfElse Exp Stmt Stmt       -- if/then/else
	        | If Exp Stmt                -- if/then/else
          | While Exp Stmt             -- while
          | Block [Stmt]               -- bloco de instruções
          | Decl (Ident, Type)         -- declaração de variáveis
          | Return Exp                 -- valor de retorno da função
          | Skip                       -- comando vazio
          | PrintINT Exp               -- comando print para INT
          | Break
          | Continue
          | For Stmt Exp Stmt Stmt     --for
          | Incr Ident
          | Decr Ident
          deriving Show

-- functions
data Fun = FunDef Ident Type ArgList [Stmt]
   deriving Show
           
-- functions arguments
type ArgList = [(Ident, Type)]

-- expressions
data Exp = Var Ident                  -- x, y, z, etc.
         | Num Int                    -- 123, etc.
         | BoolVal Boolean            -- 'true', 'false'
         | OpBin BinOp Exp Exp        -- e1+e2, e1*e2, ...
	 | OpRel RelOp Exp Exp        -- e1<e2, e1!=e2, ...
         | Not Exp                    -- NOT operator
	 | FunCall Ident [Exp]        -- function call and its arguments
	 deriving (Eq, Show)

-- arithmetic operators
data BinOp = Plus
           | Minus
           | Times
           | Div
           | Mod
           deriving (Eq, Show)

-- relational operators
data RelOp = Lteq
           | Lt
           | Gt
           | Gteq
           | Diff
           | Eq
           | And
           | Or
	   deriving (Eq, Show)
  
parseError :: [Token] -> a
parseError toks = error "parse error"
}
