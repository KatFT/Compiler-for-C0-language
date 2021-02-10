{
module Lexer where
}

%wrapper "basic"

$white = [\ \t\n\r\b\v\f\a\\\'\"]
$alpha = [_A-Za-z]
$digit = [0-9]

tokens :-

$white+                                                 ;                      -- ignorar caracteres brancos
"//".*                                                  ;                      -- ignorar comentários na mesma linha
"/*"([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))*"*"+"/"       ;                      -- ignorar comentários multi linha
"+"                                          { \_ -> PLUS }
"-"                                          { \_ -> MINUS }
"*"	                                     { \_ -> MULT }
"/"                                          { \_ -> DIV }
"%"                                          { \_ -> MOD }
"="                                          { \_ -> ASSIGN }
">"                                          { \_ -> GREATER }
"<"                                          { \_ -> LESS }
">="                                         { \_ -> GREATERorEQUAL }
"<="                                         { \_ -> LESSorEQUAL }
"!="                                         { \_ -> DIFF }
"=="                                         { \_ -> EQUAL }
"("                                          { \_ -> LPAREN }
")"                                          { \_ -> RPAREN }
"{"                                          { \_ -> LBRACE }
"}"                                          { \_ -> RBRACE }
";"                                          { \_ -> SEMICOLON }
","                                          { \_ -> COMMA }
"!"                                          { \_ -> NOT }
"&&"                                         { \_ -> AND}
"||"                                         { \_ -> OR } 
"true"                                       { \_ -> TRUE }
"false"                                      { \_ -> FALSE }
"bool"                                       { \_ -> BOOL }
"int"                                        { \_ -> INT }
"print_int"                                  { \_ -> PRINTint }
if		                             { \_ -> IF }
else                                         { \_ -> ELSE }
return                                       { \_ -> RETURN }
break                                        { \_ -> BREAK }
continue                                     { \_ -> CONTINUE }
while                                        { \_ -> WHILE }
for                                          { \_ -> FOR }
$alpha($alpha|$digit)*	                     { \s -> ID s }
$digit+                                      { \s -> NUM (read s) }
{
data Token = ID String | NUM Int | GREATER | LESS | GREATERorEQUAL | LESSorEQUAL | DIFF | EQUAL | ASSIGN | IF | ELSE | WHILE | INT | BOOL | PLUS | DIV | MINUS | MOD | MULT | SEMICOLON | COMMA | RPAREN | LPAREN | RBRACK | LBRACK | RBRACE | LBRACE | RETURN | PRINTint | NOT | AND | OR | BREAK | CONTINUE | FOR | TRUE | FALSE
           deriving (Eq, Show)
}
