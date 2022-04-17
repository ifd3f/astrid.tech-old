# Example content directory

This directory is an example of a content directory. It also provides happy path cases for testing the code with.

## Documents

The fundamental unit that Seams reads is the document. Seams only supports two kinds of documents:

- YAML (`*.yml|*.yaml`)
- Markdown (`*.md`, and specifically just the frontmatter, not the content)

The file extension is the only piece of data that Seams determines from the path. All the required data is fully encapsulated inside the data file. 

What that means for you is, the way you organize your content is completely up to you, so long as all documents are located inside the content folder. 

(Note: this path-agnostic business was brought about by a lot of pain from writing path-reading code in the past.)

## URL slugs

However, what Seams *is* opinionated about is the URL slugs. URL slugs are not determined by path, but by what it says inside the file. They must be of the form

```hs
[Int, Int, Int, Int]
```

or 

```hs
[Int, Int, Int, Int, String]
```

## Content sourcing

Given a document, Seams will search for the associated content in the following order:

1. If the document is a Markdown frontmatter (i.e. the source path ends in `.md`), then the content is the Markdown after the frontmatter.
2. If the `content` property is a string, then treat that string as the plaintext content.
3. Otherwise, strip the file extension (i.e. `somefile.html.yaml` becomes `somefile.html`) and treat the file at the new path as the content.

Note that if the `content` property exists on a Markdown frontmatter, the Seams importer will ignore `content` and only look at the Markdown.

