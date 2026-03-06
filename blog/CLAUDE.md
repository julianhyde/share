# Blog CLAUDE.md

Julian Hyde's Jekyll blog at `http://blog.hydromatic.net`.

## Directory structure

```
blog/
  _posts/          # Published posts (Markdown)
  _drafts/         # Draft posts (not published)
  assets/img/      # Images
  _config.yml      # Jekyll site config
  Gemfile          # Ruby dependencies
  docker-compose.yml
  after.sh         # Post-build syntax highlighting fixup
```

## Creating a new post

### File naming

Posts go in `blog/_posts/` with the filename pattern:

```
YYYY-MM-DD-short-title-slug.md
```

Use today's date and a lowercase, hyphenated slug derived from the title.

### Front matter

Every post requires this front matter block at the top:

```yaml
---
layout: post
title:  "Title of the Post"
date:   YYYY-MM-DD 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
---
```

- `image`: Use the morel mushroom image for Morel-related posts. For other topics, pick a relevant image from `assets/img/`.
- `tweet`: Fill in after publishing the tweet. Use `XXXXXXXXXXXXXXXXX` as a placeholder until then.
- `date`: Set to noon Pacific time (`12:00:00 -0800`).

### Post footer

End every post with the social/comments footer and the history link:

```markdown
If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
-->
```

### Code blocks

Use standard Markdown fenced code blocks with a language tag:

- Morel / Standard ML: ` ```sml `
- Datalog / Souflé: ` ```prolog `
- SQL: ` ```sql `
- Shell: ` ```bash `

For Morel output lines, wrap them in `(*[` ... `]*)` comment markers so `after.sh` can gray them out:

```sml
val x = 42;
(*[> val x = 42 : int]*)
```

### Morel keyword highlighting

`after.sh` post-processes the built HTML to highlight Morel-specific keywords (`from`, `where`, `yield`, `group`, `compute`, `join`, `exists`, `elem`, `orelse`, `andalso`, etc.) that are not highlighted by the standard SML syntax highlighter. This runs automatically as part of the Docker build.

## Building and previewing

### Dev server (live reload)

```bash
cd blog
docker compose run --service-ports dev
```

Then open `http://localhost:4000/draft-blog/`.

### Production build

```bash
cd blog
docker compose run build-site
bash after.sh
```

The built site is in `blog/_site/`.

## Publishing workflow

1. Create the post file in `blog/_posts/YYYY-MM-DD-slug.md`.
2. Preview with the dev server.
3. Commit the file: `git add blog/_posts/YYYY-MM-DD-slug.md && git commit`.
4. Push to `master` (the main branch for this repo): `git push origin master`.
5. Post on Bluesky and/or Twitter/X, then update the `tweet:` field in the front matter with the tweet URL and commit again.

## Config notes

- **Do not commit `_config.yml`**. The working copy uses `baseurl: "/draft-blog"` for local
  previewing; the committed version uses `baseurl: "/"` for production. Switch back before
  committing if you accidentally changed it.
- `timezone: America/Los_Angeles` — post dates are interpreted in Pacific time.
- The `jekyll-twitter-plugin` renders the `{% twitter %}` Liquid tag using the `tweet:` front matter field.
