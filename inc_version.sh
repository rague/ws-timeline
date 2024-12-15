#! /bin/zsh

old=$(cat version)
version=$(($old + 1))
echo $version >version

sed -i '' -e "s/version=$old/version=$version/g" start-duration/index.html
sed -i '' -e "s/version=$old/version=$version/g" start-duration-mn/index.html
sed -i '' -e "s/version=$old/version=$version/g" start-end/index.html
sed -i '' -e "s/version=$old/version=$version/g" pdf-gen/index.html

