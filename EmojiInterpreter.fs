module EmojiInterpreter

open System.Collections.Generic
open EmojiParser

// Определение стандартных арифметических операций
let standardFunctions = 
    [
        ("➕", fun args -> 
            match args with
            | [IntLiteral a; IntLiteral b] -> IntLiteral (a + b)
            | _ -> failwith "Некорректные аргументы для операции сложения")
        
        ("➖", fun args -> 
            match args with
            | [IntLiteral a; IntLiteral b] -> IntLiteral (a - b)
            | _ -> failwith "Некорректные аргументы для операции вычитания")
        
        ("✖️", fun args -> 
            match args with
            | [IntLiteral a; IntLiteral b] -> IntLiteral (a * b)
            | _ -> failwith "Некорректные аргументы для операции умножения")
        
        ("➗", fun args -> 
            match args with
            | [IntLiteral a; IntLiteral b] -> 
                if b = 0 then failwith "Деление на ноль" 
                else IntLiteral (a / b)
            | _ -> failwith "Некорректные аргументы для операции деления")
    ]

// Функция вызова стандартной операции
let applyStandardFunction name args =
    match List.tryFind (fun (op, _) -> op = name) standardFunctions with
    | Some (_, fn) -> fn args
    | None -> failwithf "Функция %s не найдена" name

// Рекурсивная функция вычисления выражений
let rec evaluateExpr expr env =
    match expr with
    | Var x -> 
        match Map.tryFind x env with
        | Some value -> value
        | None -> failwithf "Переменная %s не найдена" x
    | IntLiteral n -> IntLiteral n
    | Abstraction(id, body) -> body
    | Application(e1, e2) -> 
        let v1 = evaluateExpr e1 env
        let v2 = evaluateExpr e2 env
        apply v1 v2
    | LetIn(id, e1, e2) ->
        let result = evaluateExpr e1 env
        evaluateExpr e2 (Map.add id result env)
    | Recursion(id, e1, e2) -> 
        let recEnv = Map.add id e1 env
        evaluateExpr e2 recEnv
    | IfThenElse(cond, e1, e2) ->
        let condValue = evaluateExpr cond env
        match condValue with
        | IntLiteral(1) -> evaluateExpr e1 env
        | _ -> evaluateExpr e2 env
    | Operator(op, args) ->
        let evaluatedArgs = List.map (fun a -> evaluateExpr a env) args
        applyStandardFunction op evaluatedArgs

// Применение абстракции
and apply e1 e2 =
    match e1 with
    | Abstraction(id, body) -> evaluateExpr body (Map.ofList [(id, e2)])
    | _ -> failwith "Неверная аппликация функции"

// Функция вычисления программы
let evaluate exp = evaluateExpr exp Map.empty

// Пример использования
let exampleProgram = 
    "💡x=5➕3"
printfn "Результат теста номер один = %A" (parse exampleProgram |> evaluate)

let exampleProgram2 = 
    "💡x=5➕3➖2✖️4➗2"
printfn "Результат теста номер два = %A" (parse exampleProgram2 |> evaluate)
