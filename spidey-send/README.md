# Spidey-Send

A crawler-based webmention sender.

## Usage

First, ensure your posts are marked up with [microformats](https://microformats.org/wiki/Main_Page), and that they are live on your website.

Then, execute the following command on a page of your website:

```sh
spidey-send https://astrid.tech/atom.xml
```

Supported link types include:

- [ ] Website pages
- [ ] Sitemaps
- [ ] RSS and Atom feeds

Calling spidey-send will create or modify a `.spidey-sent.json` file. 

Finally, commit that file to your preferred version control system so that we don't send webmentions too many times.

### Options

- `--depth <n>` - Control how deep the app crawls.
