{
module Parser where
import Ast
import Token
import Err
}

%name parse Expression
%tokentype  { Token }
%error      { parseError }

%token
    fix   { TokenFix }
    let   { TokenLet }
    rec   { TokenRec }
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    in    { TokenIn }
    int   { TokenInt $$ }
    float { TokenFloat $$ }
    var   { TokenVar $$ }
    sym   { TokenSym $$ }
    true  { TokenTrue }
    false { TokenFalse }
    '.'   { TokenPoint }
    '\\'  { TokenLambda }
    '='   { TokenAssign }
    ':'   { TokenPoints }
    '->'  { TokenArrow }
    '+'   { TokenPlus }
    '-'   { TokenMinus }
    '*'   { TokenTimes }
    '/'   { TokenDiv }
    '('   { TokenLPar }
    ')'   { TokenRPar }
    '>'   { TokenMore }
    '>='  { TokenMoreEqual }
    '<'   { TokenLess }
    '<='  { TokenLessEqual }
    '=='  { TokenEquals }
    '!='  { TokenDifferent }
    '&&'  { TokenAnd }
    '||'  { TokenOr }
    '['   { TokenLParRect }
    ']'   { TokenRParRect }
    ','   { TokenComma }

%%

TypeDeclarationAtomic :: { DeclaredType }
TypeDeclarationAtomic : var                      { DVar $1 }
                      | '[' TypeDeclaration ']'  { DList $2 }
                      | '(' TypeDeclaration ')'  { $2 }

TypeDeclaration :: { DeclaredType }
TypeDeclaration : TypeDeclarationAtomic                      { $1 }
                | TypeDeclarationAtomic '->' TypeDeclaration { $1 ::-> ($3) }

Expression :: { Term }
Expression : Conditional                                                                      { $1 }
           | '\\' var Variables '.' Expression                                                { Lambda ($2:$3) $5 }
           | '\\' var Variables ':' TypeDeclaration '.' Expression                            { TypedLambda ($2:$3) $5 $7 }

           | let var ':' Variables '=' Expression in Expression                               { Let $2 $4 $6 $8 }
           | let var ':' Variables ':' TypeDeclaration '=' Expression in Expression           { TypedLet $2 $4 $6 $8 $10 }

           | let rec var ':' Variables '=' Expression in Expression                           { LetRec $3 $5 $7 $9 }
           | let rec var ':' Variables ':' TypeDeclaration '=' Expression in Expression       { TypedLetRec $3 $5 $7 $9 $11 }

           | let '(' sym ')' ':' var var '=' Expression in Expression                         { Let $3 [$6, $7] $9 $11 }
           | let '(' sym ')' ':' var var ':' TypeDeclaration '=' Expression in Expression     { TypedLet $3 [$6, $7] $9 $11 $13 }

           | let rec '(' sym ')' ':' var var '=' Expression in Expression                     { LetRec $4 [$7, $8] $10 $12 }
           | let rec '(' sym ')' ':' var var ':' TypeDeclaration '=' Expression in Expression { TypedLetRec $4 [$7, $8] $10 $12 $14 }

           | if Expression then Expression else Expression                                    { If $2 $4 $6 }
           | fix Atomic                                                                       { Fix $2 }

Variables :: { [Identity] }
Variables : {- empty -} { [] }
          | var Variables  { $1 : $2 }

Conditional :: { Term }
Conditional : Conditional '&&' Condition { ((Prim (:&&)) :@ $1) :@ $3 }
            | Conditional '||' Condition { ((Prim (:||)) :@ $1) :@ $3 }
            | Condition                  { $1 }

Condition :: { Term }
Condition : Condition '>'  Sum { ((Prim (:>)) :@ $1) :@ $3 }
          | Condition '>=' Sum { ((Prim (:>=)) :@ $1) :@ $3 }
          | Condition '<'  Sum { ((Prim (:<)) :@ $1) :@ $3 }
          | Condition '<=' Sum { ((Prim (:<=)) :@ $1) :@ $3 }
          | Condition '==' Sum { ((Prim (:==)) :@ $1) :@ $3 }
          | Condition '!=' Sum { ((Prim (:!=)) :@ $1) :@ $3 }
          | Sum                { $1 }

Sum :: { Term }
Sum : Sum '+' Multiplication { ((Prim (:+)) :@ $1) :@ $3 }
    | Sum '-' Multiplication { ((Prim (:-)) :@ $1) :@ $3 }
    | Multiplication { $1 }

Multiplication :: { Term }
Multiplication : Multiplication '*' Cons { ((Prim (:*)) :@ $1) :@ $3 }
               | Multiplication '/' Cons { ((Prim (:/)) :@ $1) :@ $3 }
               | Cons                    { $1 }

Cons :: { Term }
Cons : Cons ':' Application   { ((Prim (:::)) :@ $1) :@ $3 }
     | Application            { $1 }

Application :: { Term }
Application : Application Atomic     { $1 :@ $2 }
            | Application sym Atomic { ((Prim (Custom $2)) :@ $1) :@ $3 }
            | Atomic                 { $1 }

List :: { Term }
List : '[' ListElements ']' { Const (CList $2) }

ListElements :: { [Term] }
ListElements : {- empty -}                 { [] }
             | Expression                  { [$1] }
             | Expression ',' ListElements { $1:$3 }

Atomic :: { Term }
Atomic : AtomicConstant     { Const $1 }
       | var                { Var $1 }
       | List               { $1 }
       | '(' Expression ')' { $2 }

AtomicConstant :: { Constant }
AtomicConstant : int   { CInt $1 }
               | float { CFloat $1 }
               | true  { CBool True }
               | false { CBool False }

{
parseError :: [Token] -> a
parseError t = error $ showErr $ Parsing t
}
