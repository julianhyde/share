share
=====

Shared files, presentations, and other materials.

# How to develop the blog

Edit `blog/_config.yml` and change
```
baseurl: "/"
```
to
```
baseurl: "/draft-blog"
```

Now run in one terminal
```
cd blog
docker compose run dev
```

and in terminal another edit the `.md` file for the blog post. Each
time you save the file, the
[draft blog site](http://www.hydromatic.net/draft-blog/) will be
re-generated.

Assumes that `draft-blog` directory under the web server is a symbolic
link to the `blog/_site` directory.

# How to build the blog

If you changed `blog/_compose.yml` above, change it back. Now run

```
cd blog
docker compose run build-site
rm -rf ~/web/blog
mv _site ~/web/blog
```
