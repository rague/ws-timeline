old=$(cat version-widget)
version=$(($old + 1))
echo $version >version-widget

elm make src/Widget.elm --optimize --output public/widget.js
elm-optimize-level-2 -O3 public/widget.js --output public/widget.js
uglifyjs public/widget.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/widget.js



sed -i '' -e "s/version=$old/version=$version/g" start-duration/index.html
sed -i '' -e "s/version=$old/version=$version/g" start-duration-mn/index.html
sed -i '' -e "s/version=$old/version=$version/g" start-end/index.html
