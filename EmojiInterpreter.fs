module EmojiInterpreter

open System.Collections.Generic
open EmojiParser

let applyOperator op args =
    match op, args with
    | "➕", [IntLiteral a; IntLiteral b] -> IntLiteral(a + b)
    | "➖", [IntLiteral a; IntLiteral b] -> IntLiteral(a - b)
    | _ -> failwith "Неизвестный оператор или некорректные аргументы"

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
        applyOperator op evaluatedArgs

and apply e1 e2 =
    match e1 with
    | Abstraction(id, body) -> evaluateExpr body (Map.ofList [(id, e2)])
    | _ -> failwith "Неверная аппликация функции"

let evaluate exp = evaluateExpr exp Map.empty

// Пример выполнения программы с эмодзи
let exampleProgram = "💡 x = 5 💼 💡 y = ➕(x, 3) 💼 ➕(y, 2)"
printfn "Результат = %A" (parse exampleProgram |> evaluate)
