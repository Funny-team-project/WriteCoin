module Сервер

open Результаты

type КодПодключения = int
type СтрокаПодключения = string
type ИмяСервера = string
type ИмяФайла = string
type ДанныеПодключения =
    | КодПодключения of КодПодключения
    | СтрокаПодключения of СтрокаПодключения

type СодержимоеФайла =
    | Строка of string
    | Байты of byte
type ИконкаФайла =
    | Байты of byte
    | Путь of string
type ИзображениеСервера =
    | Байты of byte
    | Путь of string

type Сервер =
    private {
        Имя: ИмяСервера
        Изображение: ИзображениеСервера
        ДанныеПодключения: ДанныеПодключения
    }

type ПраваДоступа =
    | ТолькоЧтение
    | Изменение
    | Удаление
    | ИзменениеПрав

type Файл =
    {
        Имя: ИмяФайла
        Адрес: string
        Содержимое: СодержимоеФайла
        Иконка: ИконкаФайла
        ДатаИзменения: System.DateTime
        АвторПравки: Профиль.Профиль
    }

type Данные =
    | Файл of Файл
    | Папка of Данные

type получитьСервера = Профиль.Профиль -> Result<List<Сервер>, Ошибка>
type подключение = ДанныеПодключения -> Result<Сервер, Ошибка>
type получитьДанные = Сервер -> Result<Данные, Ошибка>
type изменитьДанные = Данные -> Result<Данные, Ошибка>
type удалитьДанные = Данные -> Result<Успех, Ошибка>
type отправитьДанные = Данные -> Сервер -> Result<Успех, Ошибка>

// частные случаи изменения данных
type переименоватьДанные = изменитьДанные
type переместитьДанные = изменитьДанные
type копироватьДанные = изменитьДанные