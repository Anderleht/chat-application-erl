# Chat Application на Erlang

Простое чат-приложение, написанное на Erlang. Проект состоит из двух приложений: клиентского (`chat_client`) и серверного (`chat_server`).

## Установка

1. Склонируйте репозиторий.
2. Перейдите в корневую папку проекта.
3. Скомпилируйте сервер с помощью команды `make compile_server`.
4. Скомпилируйте клиент с помощью команды `make compile_client`.

## Запуск

1. Для запуска сервера выполните команду `make start_server`.
2. В shell Erlang выполните команды:

    ```erlang
    chat_supervisor:start_link().
    chat_server:start().
    ```

3. Откройте новый терминал и запустите клиент с помощью команды `make start_client`.
4. В shell Erlang клиента выполните команду:

    ```erlang
    Pid = chat_client:start("user", "password").
    ```

## Команды для общения клиента с сервером

1. `chat_client:start("user", "password")` - подключение к серверу по логину и паролю, где `"user"` - логин, а `"password"` - пароль.
2. `chat_client:send(Pid, Msg)` - для отправки сообщений всем подключенным пользователям к серверу, за исключением вас.
3. `chat_client:stop(Pid)` - для закрытия соединения с сервером.

