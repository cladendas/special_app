FROM --platform=linux/amd64 haskell:9.8.4 AS builder

WORKDIR /app

RUN stack update

# Сначала копируем только конфиги для кэширования зависимостей
COPY stack.yaml stack.yaml.lock company-project.cabal /app/
RUN stack build --dependencies-only

# Затем весь код
COPY . /app
RUN stack build
RUN stack install --local-bin-path /bin

# Финальный образ
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root
COPY --from=builder /bin/company-project .

# Какой порт слушает приложение
EXPOSE 8080 
CMD ["./company-project"]


################################################################

# FROM haskell:9.8.4 as builder

# WORKDIR /app

# # Копируем файлы проекта
# COPY . .

# RUN stack upgrade

# # Собираем проект
# RUN stack install --copy-bins --skip-ghc-check

# # Второй этап - минимальный образ
# FROM ubuntu:22.04

# RUN apt-get update && apt-get install -y \
#     ca-certificates \
#     && rm -rf /var/lib/apt/lists/*

# WORKDIR /root

# # Копируем бинарник из стадии сборки
# COPY --from=builder /root/.local/bin/company-project .

# # Указываем порт (замените на ваш)
# EXPOSE 3000

# # Запускаем приложение
# CMD ["./company-project"]