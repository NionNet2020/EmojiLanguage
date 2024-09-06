module EmojiParser

open EmojiTokenizer
open System

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

let rec parseTokens tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | Token.Emoji "💡" :: Token.Identifier id :: Token.Operator "=" :: rest ->
        let expr, rest' = parseTokens rest
        (LetIn(id, expr, Var id), rest')
    | Token.Emoji "😃" :: Token.Identifier id :: Token.Emoji "->" :: rest ->
        let body, rest' = parseTokens rest
        (Abstraction(id, body), rest')
    | Token.Number n :: rest -> (IntLiteral n, rest)
    | Token.Identifier id :: rest -> (Var id, rest)
    | Token.Emoji "🌟" :: rest ->
        let cond, rest' = parseTokens rest
        let thenExpr, rest'' = parseTokens rest'
        let elseExpr, rest''' = parseTokens rest''
        (IfThenElse(cond, thenExpr, elseExpr), rest''')
    | Token.OpenParen :: rest ->
        let expr, rest' = parseTokens rest
        match rest' with
        | Token.CloseParen :: rest'' -> (expr, rest'')
        | _ -> failwith "Expected closing parenthesis"
    | Token.Operator op :: rest ->
        let exprs, rest' = parseMany rest
        (Operator(op, exprs), rest')
    | _ -> failwith "Unexpected token"

and parseMany tokens =
    match tokens with
    | Token.CloseParen :: _ -> ([], tokens)
    | _ ->
        let expr, rest = parseTokens tokens
        let exprs, rest' = parseMany rest
        (expr :: exprs, rest')

let parse input =
    let tokens = EmojiTokenizer.tokenize input
    fst (parseTokens tokens)
