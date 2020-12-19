maxsize () {
  convert $1 -resize 1600x1600\> $1
}

for file in $(find content assets -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" \) )
do
  echo $file
  maxsize $file
done
