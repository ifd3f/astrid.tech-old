# Webmention Receiver

This app receives webmentions and pushes them to Git.

## Endpoints

`/api/webmention` - The primary webmention endpoint. The server does not immediately process webmentions, but appends them to the queue to await processing.

`/api/rpc/processWebmentions` - A POST to this endpoint causes the server to process all the queued webmentions. This endpoint should be protected. It is meant to be CURLed on a cron job, like so:

```sh
curl -D - -u rpcuser:passwordhere -X POST https://api.astrid.tech/api/rpc/processWebmentions
```
