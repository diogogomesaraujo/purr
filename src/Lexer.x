{
module Lexer where

import Token
}

%wrapper "basic"

$whitespace = [ \t\n\r\f\y]
$digit      = [0-9]
$large      = [A-Z]
$small      = [a-z \_]
$alpha      = [$small $large]
$ascsymbol  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

@int   = "-"?$digit+
@float = "-"?$digit+ "." $digit+

tokens :-
    $white+                               ;
    "--".*                                ;
    fix                                   { \_ -> TokenFix }
    let                                   { \_ -> TokenLet }
    rec                                   { \_ -> TokenRec }
    op                                    { \_ -> TokenOp }
    in                                    { \_ -> TokenIn }
    if                                    { \_ -> TokenIf }
    then                                  { \_ -> TokenThen }
    else                                  { \_ -> TokenElse }
    true                                  { \_ -> TokenTrue }
    false                                 { \_ -> TokenFalse }
    fn                                    { \_ -> TokenFn }
    "->"                                  { \_ -> TokenArrow }
    "="                                   { \_ -> TokenAssign }
    ":"                                   { \_ -> TokenPoints }
    "+"                                   { \_ -> TokenPlus }
    "-"                                   { \_ -> TokenMinus }
    "/"                                   { \_ -> TokenDiv }
    "*"                                   { \_ -> TokenTimes }
    "("                                   { \_ -> TokenLPar }
    ")"                                   { \_ -> TokenRPar }
    ">"                                   { \_ -> TokenMore }
    ">="                                  { \_ -> TokenMoreEqual }
    "<"                                   { \_ -> TokenLess }
    "<="                                  { \_ -> TokenLessEqual }
    "=="                                  { \_ -> TokenEquals }
    "!="                                  { \_ -> TokenDifferent }
    "&&"                                  { \_ -> TokenAnd }
    "||"                                  { \_ -> TokenOr }
    ">="                                  { \_ -> TokenMoreEqual }
    "["                                   { \_ -> TokenLParRect }
    "]"                                   { \_ -> TokenRParRect }
    ","                                   { \_ -> TokenComma }
    @int                                  { \s -> TokenInt (read s) }
    @float                                { \s -> TokenFloat (read s) }
    [$alpha \_] [$alpha $digit \_ \']*    { \s -> TokenVar s }
    $ascsymbol [$ascsymbol]*              { \s -> TokenSym s }
