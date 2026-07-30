{
module Lexer where

import Token
}

%wrapper "basic"

$whitespace = [ \t\n\r\f\v]
$digit      = [0-9]
$large      = [A-Z]
$small      = [a-z]
$alpha      = [$small $large]
$ascsymbol  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

@int   = "-"?$digit+
@float = "-"?$digit+ "." $digit+

tokens :-
    $whitespace+                          ;
    "--".*                                ;
    fix                                   { \_ -> TokenFix }
    let                                   { \_ -> TokenLet }
    rec                                   { \_ -> TokenRec }
    in                                    { \_ -> TokenIn }
    if                                    { \_ -> TokenIf }
    then                                  { \_ -> TokenThen }
    else                                  { \_ -> TokenElse }
    true                                  { \_ -> TokenTrue }
    false                                 { \_ -> TokenFalse }
    fn                                    { \_ -> TokenFn }
    "->"                                  { \_ -> TokenArrow }
    ":"                                   { \_ -> TokenPoints }
    "+"                                   { \_ -> TokenPlus }
    "-"                                   { \_ -> TokenMinus }
    "/"                                   { \_ -> TokenDiv }
    "*"                                   { \_ -> TokenTimes }
    "("                                   { \_ -> TokenLPar }
    ")"                                   { \_ -> TokenRPar }
    ">="                                  { \_ -> TokenMoreEqual }
    "<="                                  { \_ -> TokenLessEqual }
    ">"                                   { \_ -> TokenMore }
    "<"                                   { \_ -> TokenLess }
    "=="                                  { \_ -> TokenEquals }
    "!="                                  { \_ -> TokenDifferent }
    "="                                   { \_ -> TokenAssign }
    "&&"                                  { \_ -> TokenAnd }
    "||"                                  { \_ -> TokenOr }
    "["                                   { \_ -> TokenLParRect }
    "]"                                   { \_ -> TokenRParRect }
    ","                                   { \_ -> TokenComma }
    @float                                { \s -> TokenFloat (read s) }
    @int                                  { \s -> TokenInt (read s) }
    [$alpha \_] [$alpha $digit \_ \']*    { \s -> TokenVar s }
    $ascsymbol+                           { \s -> TokenSym s }
