git checkout master
chcp 65001
stack exec site clean
stack exec site build
git add -A
git commit -m "Publish."
git push origin master:master
