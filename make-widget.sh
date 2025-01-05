old=$(cat version-widget)
version=$(($old + 1))
echo $version >version-widget

rm "public/widget-v$old.js"
rm "public/init-v$old.js"

elm make src/Widget.elm --optimize --output public/widget-v$version.js
elm-optimize-level-2 -O3 public/widget.js --output public/widget-v$version.js
uglifyjs public/widget-v$version.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/widget-v$version.js


cp "public/init.js" "public/init-v$version.js"

sed -i '' -e "s/-v$old/-v$version/g" start-duration/index.html
sed -i '' -e "s/-v$old/-v$version/g" start-duration-mn/index.html
sed -i '' -e "s/-v$old/-v$version/g" start-end/index.html
