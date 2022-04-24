# Webmention Receiver

This app receives webmentions and pushes them to Git.

## Endpoints

`/api/webmention` - The primary webmention endpoint. The server does not immediately process webmentions, but appends them to the queue to await processing.

`/api/rpc/processWebmentions` - A POST to this endpoint causes the server to process all the queued webmentions. This endpoint should be protected. It is meant to be CURLed on a cron job, like so:

```sh
curl -u rpcuser:passwordhere -X POST https://api.astrid.tech/api/rpc/processWebmentions
```

## Webmention Storage

Webmentions are stored as JSON files like so:

```json
{
  "source": "https://webmention.rocks/receive/1/02d58e7042b47d9976eefd731067190b",
  "target": "https://astrid.tech/2021/09/19/0/spurious-network-error/",
  "mentionedOn": "2021-09-19T04:26:46.096907500Z",
  "processedOn": "2021-09-19T04:26:52.091081431Z",
  "relUrl": {
    "rels": [],
    "text": "https://astrid.tech/2021/09/19/0/spurious-network-error/"
  }
}
```

Given a webmention for a certain target and source, webmentions get stored to:

```
$WEBMENTION_SUBDIR/sanitize($target)/sanitize($source)/sha256hash("$source|$target").json
```

In this case, that would be
```
$WEBMENTION_SUBDIR/https---astrid.tech-2021-09-19-0-spurious-network-error-/https---webmention.rocks-receive-1-02d58e7042b47d9976eefd731067190b/eb106d81be5e4ddaba457ff09c0323abcffe4024b076d99eebc7490c342b5731.json
```

However, in your code, you shouldn't need to calculate these values. You can instead recursively walk `WEBMENTION_SUBDIR` to get all webmentions.

## Docker container configuration

### Files of note 

| Variable            | Info                                                                                                                                                             | Mount?                            |
| ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- |
| `/app/data/data.db` | This SQLite file is automatically created to keep track of webmentions pending processing, so that they can be batch-processed on calls to `processWebmentions`. | Yes, but mount its parent folder. |
| `/app/known_hosts`  | The SSH known_hosts file to use. Required if you're using SSH to push to your git repo.                                                                          | Yes                               |
| `/app/id_rsa`       | The RSA private key to use for SSH.                                                                                                                              | Yes                               |

### Environment Variables

| Variable               | Info                                                                          | Example                                                                                                  |
| ---------------------- | ----------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| `ALLOWED_TARGET_HOSTS` | A comma-separated list of hosts to allow processing webmentions for.          | `astrid.tech,aay.tw`                                                                                     |
| `BRANCH_NAME`          | The branch to push changes to.                                                | `auto/webmention`                                                                                        |
| `BASE_BRANCH`          | The branch to base `BRANCH_NAME` off of, if `BRANCH_NAME` does not yet exist. | `main`                                                                                                   |
| `GIT_NAME`             | The name to commit code with.                                                 | `Astrid Yu`                                                                                              |
| `GIT_EMAIL`            | The email to commit code with.                                                | `astrid@astrid.tech`                                                                                     |
| `REMOTE_URL`           | The Git remote to push changes to.                                            | `https://github.com/astridyu/astrid.tech.git` or `git@github.com:astralbijection/astrid.tech.git` |
| `WEBMENTION_SUBDIR`    | A subdirectory in your git repo to store the processed webmention data in.    | `content/webmentions`                                                                                    |
