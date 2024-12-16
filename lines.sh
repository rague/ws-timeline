#! /bin/zsh
elm=$(( find src/. -name '*.elm' -print0 | xargs -0 cat ) | wc -l)
js=$((find ./(public|pdf-gen) -name '*.js' ! -name "settings.js" ! -name "widget.js" ! -name "moment.min.js" -print0 | xargs -0 cat) | wc -l)
echo $(($elm+$js))