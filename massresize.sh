maxsize () {
  convert $1 -resize 2000x2000\> $1
}

for file in $(find content assets -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" \) )
do
  echo $file
  maxsize $file
done
