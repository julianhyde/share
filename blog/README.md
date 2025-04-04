# Julian's blog site

This directory contains the sources/templates for generating Julian's blog site,
[blog.hydromatic.net](https://blog.hydromatic.net/). The actual generated content of the website
is present in the [share](https://github.com/julianhyde/share) repository.

# Previewing the website locally using docker

## Setup your environment

1. Install [docker](https://docs.docker.com/install/)
2. Install [docker compose](https://docs.docker.com/compose/install/)

## Build site

1. `cd blog`
2. In `_config.yml`, set the base: `baseurl: "/"`
3. `docker compose run build-site`
4. `./after.sh` (to highlight Morel-specific keywords)
5. Move the generated `_site` directory to `/home/jhyde/web2/blog`

## Build draft site

1. `cd blog`
2. In `_config.yml`, set the base: `baseurl: "/draft-blog"`
3. `docker compose run build-site`
4. `./after.sh` (to highlight Morel-specific keywords)
5. Move the generated `_site` directory to `/home/jhyde/web/draft-blog/`
