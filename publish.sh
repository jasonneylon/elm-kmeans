git checkout --orphan gh-pages
elm-make Main.elm --output=index.html
git add index.html
git commit -m "Creating github page"
git push --set-upstream origin gh-pages
