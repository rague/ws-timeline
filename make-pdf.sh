elm make src/PdfSettings.elm --optimize --output pdf-gen/settings.js
uglifyjs pdf-gen/settings.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output pdf-gen/settings.js

old=$(cat version-pdf)
version=$(($old + 1))
echo $version >version-pdf

sed -i '' -e "s/version=$old/version=$version/g" pdf-gen/index.html