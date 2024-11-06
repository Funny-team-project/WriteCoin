open System
open System.Threading

[<EntryPoint>]
let main argv = 
    RestAPI.Тест.тест (1)

    printfn "Исключение превзойдено"

    0