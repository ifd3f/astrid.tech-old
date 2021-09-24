# LATG - Look At This Graph

A flat-file-to-API CMS.

What this service does is:
1. Takes in a git repo full of content (see [the content directory](../content))
2. Applies all the necessary transforms to the content (i.e. Markdown/LaTeX/Jupyter to HTML) 
3. Indexes it to a Postgres DB
4. Serves it over GraphQL

## Development environment setup

Install the following:
- [DBMate](https://github.com/amacneil/dbmate) to manage migrations
- [Bazel](https://bazel.build/) to build everything
- [Docker](https://www.docker.com/) and [Docker Compose](https://docs.docker.com/compose/) to spawn the database

Then, run the following commands to prepare the database:
```sh
docker-compose -f docker-compose.dev.yaml up -d
dbmate up
```
