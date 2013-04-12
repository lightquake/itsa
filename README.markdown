# About itsa

Itsa is intended to be a simple-to-use blog engine with the following
features:

* All non-theme content is editable with any text editor
* Support for multi-language syntax highlighting
* Simple-to-understand code

I wrote it as a replacement for my old implementation of this idea,
[milagos](https://github.com/lightquake/milagos), which is written using
the Yesod framework. itsa is written using Snap, which I've found is
lighter-weight.

# Getting set up

Due to the fact that I'm using Hamlet templates, which are checked at
compile time, changing the themes requires having a Haskell toolchain
(ghc, cabal, etc.). I personally use
[hsenv](http://hackage.haskell.org/package/hsenv) for sandboxing, but
cabal-dev or whatever works just as well; just clone the repository,
set up your sandbox if you use one, and `cabal install
--only-dependencies`.

To set up a development version of the server, run `cabal install
-fdevelopment` and run `dist/build/itsa/itsa`; the development version
is slower but will autoreload. To get a production version, just omit
the `-fdevelopment`.

## Data structure

The required directory structure looks like this:

+ log/
+ config.yml
+ posts/
    + slug-1/
         + post.markdown
         + meta.yml
+ pages/
    + page-slug/
         + page.markdwon
         + meta.yml

### config.yml

The required fields of a config.yml file are:

* `posts-per-page`: The number of posts to display on a page.
* `blog-title`: The title, which will show up in various places.
* `time-zone`: The time zone to use when displaying post times.
* `app-root`: The 'base' URL for all links (i.e.,
  http://blog.amateurtopologist.com/). Trailing slashes are OK.

### .markdown

The post.markdown and page.markdown files use
[Pandoc markdown](http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html),
which is a slight superset of regular markdown. The most salient
extension is that you can do

    ```haskell
    x :: Int
    x = 3
    ```

and the code will be highlighted as Haskell (though see the
`code-language` option for an alternative).

### meta.yml

The slug is *not* in the meta.yml; instead, it is taken from the
directory name.

The meta.yml file contains all the post/page metadata. For a post, the
fields are as follos:

* `title`: The title of the post.
* `tags`: A list of the tags.
* `draft`: Whether the post is a draft; defaults to true.
* `posted`: The date at which this post was posted (if set to the
  future, the post will be queued). The format is `2012-03-27 19:00:00
  EDT`.
* `code-language`: If present, all non-inline code blocks will be
  highlighted using this language.

For pages, there are only two attribtues:

* `title`: The title of the page
* `short-title`: The 'short' title, to be used in the page
  list. Defaults to the title.
