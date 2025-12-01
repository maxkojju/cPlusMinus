#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "using: $0 [аргумент_для_compiler] [имя_выходного_файла]"
    exit 1
fi

ARG1=$1
ARG2=$2

# 1. Запуск компилятора
echo "Generatig C++ code..."
./compiler "$ARG1" > testcomp.cpp

# Проверка: 
if [ $? -ne 0 ]; then
    echo "Error: ./compiler failed"
    exit 1
fi

# 2. Компиляция  g++
echo "Compiling with g++..."
g++ testcomp.cpp -o "$ARG2"

# Проверка:
if [ $? -ne 0 ]; then
    echo "Error: g++ compilation failed"
    exit 1
fi

# 3. Запуск полученной программы
echo "Running output..."
echo "-------------------"
./"$ARG2"
