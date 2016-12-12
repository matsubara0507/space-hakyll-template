---
title:  "Welcome to Hakyll!"
---

You’ll find this post in your `posts` directory. Go ahead and edit it and re-build the site to see your changes. You can rebuild the site in many different ways, but the most common way is to run `stack exec site serve`, which launches a web server and auto-regenerates your site when a file is updated.

To add new posts, simply add a file in the `posts` directory that follows the convention `YYYY-MM-DD-name-of-post.ext` and includes the necessary front matter. Take a look at the source for this post to get an idea about how it works.

hakyll also offers powerful support for code snippets:

```haskell
printHi :: String -> IO ()
printHi = print . mappend "Hi, "

main :: IO ()
main = printHi "Tom"
-- "Hi, Tom"
```

Check out the [Hakyll docs][hakyll-docs] for more info on how to get the most out of Hakyll. File all bugs/feature requests at [Hakyll’s GitHub repo][hakyll-gh]. If you have questions, you can ask them on [Hakyll Talk][hakyll-talk].

[hakyll-docs]: https://jaspervdj.be/hakyll/reference
[hakyll-gh]:   https://github.com/jaspervdj/hakyll
[hakyll-talk]: https://groups.google.com/group/hakyll
