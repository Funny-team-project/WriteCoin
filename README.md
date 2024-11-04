# FileMessenger

## О проекте

Децентрализованный файлообменник-мессенджер.
В приложении есть профили пользователей. Вход по логину и паролю. В профиль можно добавить сервер (указать ip адрес, порт, логин и пароль для авторизации на сервере). На сервер можно отправлять файлы в выбранную папку. Также можно добавить другого пользователя для отправки файлов на этот сервер. Есть чат для общения между админами сервера. Есть история отправки файлов и uptime сервера.
У проекта есть серверная часть, которая будет обязательна для добавления на все сервера, которые пользователь будет добавлять для достижения взаимной работы.

Frontend-часть общается с Backend по REST API. Backend написан в стиле микросервисной архитектуры. Взаимодействие между внутренними сервисами также происходит по REST API.