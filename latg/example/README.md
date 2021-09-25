# Example content directory

This directory is an example of a content directory. 

## Documents

The fundamental unit that LATG reads is the document.  There are 4 kinds of document types supported:

- YAML (`*.yml|*.yaml`)
- JSON (`*.json`)
- TOML (`*.toml`)
- Markdown (`*.md`, and specifically just the frontmatter, not the content)

A document must have a `docType` property to be considered a document, or a `contentType` property to be considered content. If a document has neither of these types and is not specifically ignored in the `.docignore`, then the entire content folder is invalid. This is to prevent the case where you accidentally leave out one of those properties, and make it explicit.

The file extension is the only piece of data that LATG determines from the path. All the required data is fully encapsulated inside the data file. 

What that means for you is, the way you organize your content is completely up to you, so long as all documents are located inside the content folder. 

(Note: this path-agnostic business was brought about by a lot of pain from writing path-reading code in the past.)

## URL slugs

However, what LATG *is* opinionated about is the URL slugs. URL slugs are not determined by path, but by what it says inside the file. They must be of the form

```hs
[Int, Int, Int, Int]
```

or 

```hs
[Int, Int, Int, Int, String]
```

## Content sourcing

Given a document, LATG will search for the associated content in the following order:

1. If the document is a Markdown frontmatter (i.e. the source path ends in `.md`), then the content is the Markdown after the frontmatter.
2. If the `content.src` property is specified, then treat that as a relative path to the content file.
3. If the `content` property is a string, then treat that string as the plaintext content.
4. Otherwise, strip the file extension (i.e. `somefile.html.yaml` becomes `somefile.html`) and treat the file at the new path as the content.

Note that if the `content` property exists on a Markdown frontmatter, the LATG importer will fail.