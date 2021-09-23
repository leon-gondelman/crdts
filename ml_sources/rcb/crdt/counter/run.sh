#!/bin/bash


EXE="~/repositories/aneris/crdts/_build/default/ml_sources/rcb/crdt/counter/counter_runner.exe"

RUN () {
   osascript -e 'tell app "Terminal" to do script "'"${EXE//\"/\\\"} ${1//\"/\\\"}  ${2//\"/\\\"}  ${3//\"/\\\"}  ${4//\"/\\\"} "'"'
}

RUN 0 2023 2024 1025
RUN 1 2023 2024 1025
RUN 2 2023 2024 1025
