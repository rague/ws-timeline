#! /bin/zsh
( find src/. -name '*.elm' -print0 | xargs -0 cat ) | wc -l
