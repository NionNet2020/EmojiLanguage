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
    let rec loop (input: string) (currentToken: string) tokens =
        if String.IsNullOrEmpty(input) then
            if currentToken = "" then tokens
            else Identifier currentToken :: tokens
        else
            let firstChar = input.[0].ToString()  // Берем первый символ как строку
            let remaining = input.Substring(1)   // Оставшаяся строка

            match firstChar with
            | " " | "\t" | "\n" | "\r" -> 
                if currentToken = "" then loop remaining "" tokens
                else loop remaining "" (Identifier currentToken :: tokens)
            | c when Char.IsDigit(c.[0]) ->
                let matchResult = Regex.Match(input, @"^\d+")
                if matchResult.Success then
                    let number = Int32.Parse(matchResult.Value)
                    let rest = input.Substring(matchResult.Length)
                    loop rest "" (Number number :: tokens)
                else
                    loop remaining (currentToken + firstChar) tokens
            | "😊" -> loop remaining "" (OpenParen :: tokens)
            | "😞" -> loop remaining "" (CloseParen :: tokens)
            | "😃" -> loop remaining "" (Emoji "😃" :: tokens)
            | "🌟" -> loop remaining "" (Emoji "🌟" :: tokens)
            | "🌞" -> loop remaining "" (Emoji "🌞" :: tokens)
            | "🌧" -> loop remaining "" (Emoji "🌧" :: tokens)
            | "💡" -> loop remaining "" (Emoji "💡" :: tokens)
            | "💼" -> loop remaining "" (Emoji "💼" :: tokens)
            | "🔄" -> loop remaining "" (Emoji "🔄" :: tokens)
            | "➕" -> loop remaining "" (Operator "➕" :: tokens)
            | "➖" -> loop remaining "" (Operator "➖" :: tokens)
            | "✖️" -> loop remaining "" (Operator "✖️" :: tokens)
            | "➗" -> loop remaining "" (Operator "➗" :: tokens)
            | _ -> loop remaining (currentToken + firstChar) tokens

    // Преобразуем входную строку в токены
    loop input "" [] |> List.rev

// Пример использования
let input = "😊 123 😃 ➕ 456"
let tokens = tokenize input
printfn "%A" tokens
