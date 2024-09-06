module EmojiTokenizer

open System
open System.Text.RegularExpressions

type Token =
    | Emoji of string
    | Identifier of string
    | Number of int
    | Operator of string
    | OpenParen
    | CloseParen
    | Invalid of string

let tokenize input =
    let rec loop chars currentToken tokens =
        match chars with
        | [] ->
            if currentToken = "" then tokens
            else Identifier currentToken :: tokens
        | c :: cs when Char.IsWhiteSpace(c) ->
            if currentToken = "" then loop cs "" tokens
            else loop cs "" (Identifier currentToken :: tokens)
        | c :: cs when Char.IsDigit(c) ->
            let token = Regex.Match(new string(c :: cs |> Array.ofList), @"^\d+").Value
            let number = Int32.Parse(token)
            loop (cs |> List.skip (token.Length - 1)) "" (Number number :: tokens)
        | c :: cs when c = "😊" -> loop cs "" (OpenParen :: tokens)
        | c :: cs when c = "😞" -> loop cs "" (CloseParen :: tokens)
        | c :: cs when c = "😃" -> loop cs "" (Emoji "😃" :: tokens)
        | c :: cs when c = "🌟" -> loop cs "" (Emoji "🌟" :: tokens)
        | c :: cs when c = "🌞" -> loop cs "" (Emoji "🌞" :: tokens)
        | c :: cs when c = "🌧" -> loop cs "" (Emoji "🌧" :: tokens)
        | c :: cs when c = "💡" -> loop cs "" (Emoji "💡" :: tokens)
        | c :: cs when c = "💼" -> loop cs "" (Emoji "💼" :: tokens)
        | c :: cs when c = "🔄" -> loop cs "" (Emoji "🔄" :: tokens)
        | c :: cs when c = "➕" -> loop cs "" (Operator "➕" :: tokens)
        | c :: cs when c = "➖" -> loop cs "" (Operator "➖" :: tokens)
        | c :: cs when c = "✖️" -> loop cs "" (Operator "✖️" :: tokens)
        | c :: cs when c = "➗" -> loop cs "" (Operator "➗" :: tokens)
        | c :: cs -> loop cs (currentToken + string c) tokens
    loop (input |> List.ofSeq) "" [] |> List.rev
