--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Highlighting.Kate (styleToCss)
import           Text.Highlighting.Kate.Styles
import           Data.Yaml.YamlLight
import           Data.ByteString.UTF8 (toString)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  configYaml <- parseYamlFile "config.yaml"
  let siteCtx = mkSiteCtx configYaml
  hakyllWith config $ do
    match ("templates/*" .||. "includes/*") $ compile templateBodyCompiler

    create ["assets/css/highlight.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss $ styleToCss espresso)

    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" . version "raw" $ do
        route $ setExtension "html"
        compile getResourceBody

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          posts <- take 4 <$> (recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw"))
          let
            postCtx' = mconcat [listField "posts" postCtx (return posts), postCtx, siteCtx]

          pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"  postCtx'
              >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ do
          posts <- take 4 <$> (recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw"))
          let
            aboutCtx = mconcat [listField "posts" postCtx (return posts), siteCtx, defaultContext]

          pandocCompiler
              >>= loadAndApplyTemplate "templates/about.html" aboutCtx
              >>= loadAndApplyTemplate "templates/minimal.html" aboutCtx
              >>= relativizeUrls

    create ["posts.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
          let archiveCtx =
                listField "posts" postCtx (return posts) `mappend`
                constField "title" "Archives" `mappend`
                siteCtx `mappend`
                defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
              >>= relativizeUrls

    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- take 4 <$> (recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw"))
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              boolField "isIndex" (const True) `mappend`
              siteCtx `mappend`
              defaultContext

        makeItem ""
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    create ["sitemap.xml"] $ do
         route idRoute
         compile $ do
           posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "raw")
           let sitemapCtx =
                 listField "entries" (postCtx `mappend` siteCtx) (return posts)

           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "time" "%Y-%m-%d" `mappend`
  dateField "date" "%b %-d, %Y" `mappend`
  defaultContext

mkSiteCtx :: YamlLight -> Context String
mkSiteCtx = mconcat . fmap mkSiteCtx' . getTerminalsKeys
  where
    mkSiteCtx' (val, [YStr key]) = constField (toString key) (toString val)
    mkSiteCtx' _ = mempty

config :: Configuration
config = defaultConfiguration
    { deployCommand = "git checkout source" `mappend`
                      "&& stack exec site rebuild" `mappend`
                      "&& git checkout master" `mappend`
                      "&& rsync -a --filter='P _site/'" `mappend`
                      " --filter='P _cache/' --filter='P .git/'" `mappend`
                      " --filter='P .stack-work' --filter='P .gitignore'" `mappend`
                      " --delete-excluded _site/ ." `mappend`
                      "&& cp -a _site/. ." `mappend`
                      "&& git add -A" `mappend`
                      "&& git commit -m 'Publish'" `mappend`
                      "&& git checkout source"
    }
