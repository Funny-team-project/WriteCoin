namespace RestAPI

module RestAPI =
    type Метод =
        | GET
        | POST
        | PUT
        | PATCH
        | DELETE
        | HEAD
        | OPTIONS
        | TRACE

    let строкаМетода (метод: Метод) : string =
        match метод with
            | GET -> "GET"
            | POST -> "POST"
            | PUT -> "PUT"
            | PATCH -> "PATCH"
            | DELETE -> "DELETE"
            | HEAD -> "HEAD"
            | OPTIONS -> "OPTIONS"
            | TRACE -> "TRACE"
    
    type ВерсияHTTP =
        | V1_0
        | V1_1
        | V2_0
        | Другая of string

    let строкаВерсииHTTP (версия: ВерсияHTTP) : string =
        match версия with
            | V1_0 -> "1.0"
            | V1_1 -> "1.1"
            | V2_0 -> "2.0"
            | Другая строка -> строка

    let строкаОтветаВерсииHTTP (версия: ВерсияHTTP) : string =
        match версия with
            | строка -> sprintf "HTTP/%s" (строкаВерсииHTTP версия)

    type КлючЗаголовкаЗапроса =
        | ContentType
        | Authorization
        | UserAgent
        | Accept
        | Другой of string

    let строкаКлючаЗаголовкаЗапроса (ключ: КлючЗаголовкаЗапроса) : string =
        match ключ with
            | ContentType -> "Content-Type"
            | Authorization -> "Authorization"
            | UserAgent -> "User-Agent"
            | Accept -> "Accept"
            | Другой строка -> строка

    type КлючЗаголовкаОтвета =
        | ContentType
        | UserAgent
        | Другой of string

    let строкаКлючаЗаголовкаОтвета (ключ: КлючЗаголовкаОтвета) : string =
        match ключ with
            | ContentType -> "Content-Type"
            | UserAgent -> "User-Agent"
            | Другой строка -> строка

    type ЗначениеЗаголовкаЗапроса =
        | Другой of string

    type ЗначениеЗаголовкаОтвета =
        | Другой of string

    type ЗаголовокЗапроса = КлючЗаголовкаЗапроса * ЗначениеЗаголовкаЗапроса
    type ЗаголовокОтвета = КлючЗаголовкаОтвета * ЗначениеЗаголовкаОтвета

    type Сообщение<'T> =
        | Строка of string
        | Структура of 'T

    type Запрос<'T> =
        {
            Метод: Метод
            URL: string
            ВерсияHTTP: ВерсияHTTP option
            Заголовки: ЗаголовокЗапроса list
            Сообщение: Сообщение<'T>
        }

    exception Ошибка of string

    type ИзвестныйКодСтатуса =
        | Continue = 100
        | SwitchingProtocols = 101
        | OK = 200
        | Created = 201
        | Accepted = 202
        | NonAuthoritativeInformation = 203
        | NoContent = 204
        | ResetContent = 205
        | PartialContent = 206
        | MultipleChoices = 300
        | MovedPermanently = 301
        | Found = 302
        | SeeOther = 303
        | NotModified = 304
        | UseProxy = 305
        | TemporaryRedirect = 307
        | BadRequest = 400
        | Unauthorized = 401
        | PaymentRequired = 402
        | Forbidden = 403
        | NotFound = 404
        | InternalServerError = 500
        | NotImplemented = 501
        | BadGateway = 502
        | ServiceUnavailable = 503
        | GatewayTimeout = 504

    type КодСтатуса =
        private
        | Информационный of int
        | Успеха of int
        | Перенаправления of int
        | ОшибкаКлиента of int
        | ОшибкаСервера of int
        static member Of (код: int) =
            match код with
                | код when код >= 100 && код <= 199 -> Ok (Информационный код)
                | код when код >= 200 && код <= 299 -> Ok (Успеха код)
                | код when код >= 300 && код <= 399 -> Ok (Перенаправления код)
                | код when код >= 400 && код <= 499 -> Ok (ОшибкаКлиента код)
                | код when код >= 500 && код <= 599 -> Ok (ОшибкаСервера код)
                | код -> Error ( Ошибка (sprintf "Некорректный код статуса %s" (код.ToString ())))

    type Ответ<'T> =
        {
            ВерсияHTTP: ВерсияHTTP option
            КодСтатуса: КодСтатуса
            Заголовки: ЗаголовокОтвета list
            Сообщение: Сообщение<'T>
        }

    type запрос<'T1, 'T2> = Запрос<'T1> -> Ответ<'T2>
    type обработкаЗапроса<'T1, 'T2> = запрос<'T1, 'T2>

module Тест =
    let тест (a: int) =
        if a = 1 then 
            Error (new System.Exception "Новое исключение")
        else
            Ok a