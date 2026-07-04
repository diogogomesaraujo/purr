{
module Parser where
import Ast
import Token
import Err
}

%name parse
%tokentype { Token }
%error     { parseError }

%token
    fix   { TokenFix }
    let   { TokenLet }
    if    { TokenIf }
    then  { TokenThen }
    else  { TokenElse }
    fn    { TokenFn }
    in    { TokenIn }
    int   { TokenInt $$ }
    float { TokenFloat $$ }
    var   { TokenVar $$ }
    true  { TokenTrue }
    false { TokenFalse }
    ':='  { TokenAssign }
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

%%

Expression :: { Term }
Expression : Conditional                                   { $1 }
           | fn var '->' Expression                        { Lambda $2 $4 }
           | let var ':=' Expression in Expression         { Let $2 $4 $6 }
           | if Expression then Expression else Expression { If $2 $4 $6 }
           | fix Atomic                                    { Fix $2 }

Conditional :: { Term }
Conditional : Conditional '&&' Condition { App (App (Prim (:&&)) $1) $3 }
            | Conditional '||' Condition { App (App (Prim (:||)) $1) $3 }
            | Condition                  { $1 }

Condition :: { Term }
Condition : Condition '>'  Sum { App (App (Prim (:>)) $1) $3 }
          | Condition '>=' Sum { App (App (Prim (:>=)) $1) $3 }
          | Condition '<'  Sum { App (App (Prim (:<)) $1) $3 }
          | Condition '<=' Sum { App (App (Prim (:<=)) $1) $3 }
          | Condition '==' Sum { App (App (Prim (:==)) $1) $3}
          | Condition '!=' Sum { App (App (Prim (:!=)) $1) $3 }
          | Sum                { $1 }

Sum :: { Term }
Sum : Sum '+' Multiplication { App (App (Prim (:+)) $1) $3 }
    | Sum '-' Multiplication { App (App (Prim (:-)) $1) $3 }
    | Multiplication { $1 }

Multiplication :: { Term }
Multiplication : Multiplication '*' Application { App (Prim (:*)) (App $1 $3) }
               | Multiplication '/' Application { App (Prim (:/)) (App $1 $3) }
               | Application                    { $1 }

Application :: { Term }
Application : Application Atomic { App $1 $2 }
            | Atomic             { $1 }

Atomic :: { Term }
Atomic : AtomicConstant     { Const $1 }
       | var                { Var $1 }
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
