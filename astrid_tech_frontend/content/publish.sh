cd $1

git init
git add .
git commit "Website updates"
git remote set origin "git@github.com:astralbijection/astralbijection.github.io.git"
git push -uf origin master