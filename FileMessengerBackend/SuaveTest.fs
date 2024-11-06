open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Writers

// Мидлвар для установки кодировки UTF-8
// let setUtf8Encoding (next : WebPart) : WebPart =
//     fun ctx ->
//         ctx.response.headers <- ("Content-Type", "text/plain; charset=utf-8") :: ctx.response.headers
//         next ctx

let сервер () =
    let customMimeTypes =
        function
        | ".avi" -> createMimeType "video/avi" false
        | _ -> None

    let mimeTypes =
        defaultMimeTypesMap @@ customMimeTypes

    // let mimeTypes =
    //     defaultMimeTypesMap
    //         @@ (function | ".html" -> createMimeType "text/plain; charset=utf-8" true | _ -> None)

    // let conf = { defaultConfig with mimeTypesMap = }
    // let app = (Successful.OK "Привет, мир!")
    let app =
        choose [
            GET >=> path "/video" >=> setMimeType "video/avi" >=> OK "Здесь будет ваше видео"
            GET >=> path "/" >=> setMimeType "text/plain; charset=utf-8" >=> OK "Привет, мир!"
        ]
    startWebServer defaultConfig app