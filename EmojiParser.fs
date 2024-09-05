module EmojiParser

open System
open FParsec

type Identifier = string

type Expression =
    | Var of Identifier
    | Abstraction of Identifier * Expression
    | Application of Expression * Expression
    | IntLiteral of int
    | IfThenElse of Expression * Expression * Expression
    | LetIn of Identifier * Expression * Expression
    | Recursion of Identifier * Expression * Expression
    | Operator of Identifier * Expression list

let ws = spaces
let str_ws s = pstring s .>> ws
let parenthesized p = between (str_ws "😊") (str_ws "😞") p

let pIdentifier = many1SatisfyL (fun c -> Char.IsLetterOrDigit(c) || c = '_') "identifier" .>> ws

let pIntLiteral = pint32 .>> ws |>> IntLiteral

let pVariable = pIdentifier |>> Var

let pAbstraction =
    pipe2 (str_ws "😃" >>. pIdentifier)
          (str_ws "->" >>. pExpression)
          (fun id body -> Abstraction(id, body))

let pApplication =
    pipe2 (pAbstraction <|> pVariable <|> pIntLiteral <|> parenthesized pExpression)
          (pAbstraction <|> pVariable <|> pIntLiteral <|> parenthesized pExpression)
          (fun e1 e2 -> Application(e1, e2))

let pIfThenElse =
    pipe3 (str_ws "🌟" >>. pExpression)
          (str_ws "🌞" >>. pExpression)
          (str_ws "🌧" >>. pExpression)
          (fun cond t f -> IfThenElse(cond, t, f))

let pLetIn =
    pipe3 (str_ws "💡" >>. pIdentifier .>> str_ws "=" >>. pExpression)
          (str_ws "💼" >>. pExpression)
          (fun id expr body -> LetIn(id, expr, body))

let pRecursion =
    pipe3 (str_ws "🔄" >>. pIdentifier .>> str_ws "=" >>. pAbstraction)
          (str_ws "💼" >>. pExpression)
          (fun id func body -> Recursion(id, func, body))

let pOperator =
    pipe2 (pIdentifier .>> ws)
          (sepBy pExpression ws)
          (fun op args -> Operator(op, args))

let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit>()

do pExpressionRef :=
    choice [
        attempt pApplication
        attempt pIntLiteral
        attempt pVariable
        attempt pIfThenElse
        attempt pLetIn
        attempt pRecursion
        attempt pOperator
        parenthesized pExpression
    ]

let parse str =
    match run pExpression str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg
