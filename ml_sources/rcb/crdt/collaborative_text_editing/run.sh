#!/bin/bash


EXE="~/Documents/uniProjekter/crdts/_build/default/ml_sources/rcb/crdt/collaborative_text_editing/finite_values_runner.exe"

RUN () {
   osascript -e 'tell application "Terminal" to do script "'"${EXE//\"/\\\"} ${1//\"/\\\"}  ${2//\"/\\\"}  ${3//\"/\\\"}  ${4//\"/\\\"} "'"'
}

RUN 0 2023 2024 1025
RUN 1 2023 2024 1025
RUN 2 2023 2024 1025
