module Профиль

open Результаты
open System.Text.RegularExpressions


type Имя = string
type Почта = string
type Пароль = string
type Изображение =
    | Байты of byte[]
    | Путь of string
type Логин =
    | Имя of Имя
    | Почта of Почта

type Профиль =
    private {
        Имя: Имя
        Почта: Почта
        Пароль: Пароль
        Изображение: Изображение
    }

type ДанныеАвторизации =
    {
        Логин: Логин
        Пароль: Пароль
    }

type ДанныеРегистрации =
    {
        Имя: Имя
        Почта: Почта
        Изображение: Изображение
        Пароль: Пароль
        ПарольПовторно: Пароль
    }


type получитьПрофили = unit -> Result<List<Профиль>, Ошибка>
type авторизация = ДанныеАвторизации -> Result<Профиль, Ошибка>
type регистрация = ДанныеРегистрации -> Result<Профиль, Ошибка>
type переключитьПрофиль = Профиль -> Result<Профиль, Ошибка>
type покинутьПрофиль = Профиль -> Result<Успех, Ошибка>
type изменитьПрофиль = Профиль -> Result<Профиль, Ошибка>
type удалитьПрофиль = Профиль -> Result<Успех, Ошибка>

let валидация (данные: ДанныеРегистрации) : Result<bool, Ошибка> =
    // проверка имени
    let имя = данные.Имя
    let имяВерно = имя.Length >= 1 && имя.Length <= 64

    // проверка почты
    let почтаВерна =
        try
            let _ = System.Net.Mail.MailAddress(данные.Почта)
            true
        with
            | :? System.FormatException -> false

    // проверка пароля
    let пароль = данные.Пароль
    let парольВерен =
        пароль.Length >= 8 &&
        Regex.IsMatch(пароль, @"\d") &&
        Regex.IsMatch(пароль, @"[A-Z]") &&
        пароль |> Seq.exists (fun c -> not (System.Char.IsLetterOrDigit c)) &&
        данные.ПарольПовторно = пароль
    
    if not имяВерно then Result.Error "Имя не удовлетворяет ограничению"
    elif not почтаВерна then Result.Error "Почта введена некорректно"
    elif not парольВерен then Result.Error "Пароль введен некорректно"
    else Ok true