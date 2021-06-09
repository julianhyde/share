share
=====

Shared files, presentations, and other materials.

# How to build the blog

```
rvm use 2.7.1
cd blog
git checkout main
bundle install
bundle exec jekyll build
rm -rf ~/web/blog
mv _site ~/web/blog

git checkout draft
bundle install
bundle exec jekyll build
rm -rf ~/web/blog/draft
mv _site ~/web/blog/draft
```
