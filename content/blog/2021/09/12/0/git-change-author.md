---
title: Changing author name and email in Git
description: 🏳️‍⚧️
date: 2021-09-12 02:35:23-07:00
ordinal: 0
tags:
  - git
---

Suppose someone who formerly committed code under
`John Smith <john.smith@example.com>` now commits code under
`Jane Smith <jane.smith@example.com>`. All of her commit history is under John,
and she wants to change it to Jane.

Additionally, she used to commit with a funny email address called
`foobar@example.com`, but she doesn't like that anymore, and wants everything to
be under `jane.smith@example.com`.

## The easy way - .mailmap

There's the non-dangerous, recommended way to do it, and that's
[`.mailmap`](https://git-scm.com/docs/gitmailmap). In short, she can commit a
file called `.mailmap` at the root of your repository that looks like this:

```
Jane Smith <jane.smith@example.com> <john.smith@example.com> <foobar@example.com>
```

where the proper email she wants goes first, and any emails that she wants to
map to the proper email goes after.

Technically, all the metadata inside the historical commits will still say
`John Smith <john.smith@example.com>` or `John Smith <foobar@example.com>`.
However, in all git clients, anything with a `john.smith@example.com` or
`foobar@example.com` will be displayed as `Jane Smith <jane.smith@example.com>`.

## The rewriting history way - git filter-branch

If Jane is the sole committer to the repo, she can use the following script:

```bash
#!/bin/bash

function do_replace {
	CMD='
	OLD_EMAIL="'$1'"
	CORRECT_NAME="Jane Smith"
	CORRECT_EMAIL="jane.smith@example.com"
	if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
	then
		export GIT_COMMITTER_NAME="$CORRECT_NAME"
		export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
	fi
	if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
	then
		export GIT_AUTHOR_NAME="$CORRECT_NAME"
		export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
	fi
	'
	echo $CMD
    FILTER_BRANCH_SQUELCH_WARNING=1	git filter-branch -f --env-filter "$CMD" --tag-name-filter cat -- --branches --tags
}

git config --unset user.email
git config --unset user.name
do_replace 'john.smith@example.com'
do_replace 'foobar@example.com'
```

This script will rewrite the entire history of her repo using git filter-branch,
permanently erasing _all_ instances of `john.smith@example.com` in metadata.
However, she would need to `git push -f` to overwrite the history in Github or
Gitlab, and if anyone else is working on the code at that time, there will be
lots of fun merge conflicts to resolve.
