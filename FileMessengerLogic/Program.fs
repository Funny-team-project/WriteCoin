open Профиль
open Результаты

[<EntryPoint>]
let main argv =
    let данные: ДанныеРегистрации = {
        Имя = "Борис"
        Пароль = "PAROLPDASF1,"
        ПарольПовторно = "PAROLPDASF1,"
        Изображение = Изображение.Путь ""
        Почта = "почта@mail.ru"
    }

    match валидация данные with
        | Ok логика -> printfn "%b" логика
        | Error ошибка -> printfn "%s" ошибка
    
    0