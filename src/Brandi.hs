{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brandi
        ( run
        )
where
--------------------------------------------------------------------------------
import           Data.Monoid       ((<>))
import           Data.Text         (unpack)
import           Dhall             (Interpret, auto, input)
import           Hakyll
import           Hakyll.Images     (compressJpgCompiler, loadImage)
import           Hakyll.Web.Sass   (sassCompilerWith)
import           RIO               hiding (handle)
import           RIO.List          (headMaybe)
import           System.FilePath   (combine, dropExtension, joinPath,
                                    splitDirectories)
import           Text.Sass.Options (SassOptions (..), SassOutputStyle (..),
                                    defaultSassOptions)

--------------------------------------------------------------------------------

indexHtmlRoute :: Routes
indexHtmlRoute = customRoute $ flip combine "index.html" . dropExtension . toFilePath

rootlessRoute :: Routes
rootlessRoute = customRoute $ joinPath . drop 1 . splitDirectories . toFilePath

categoryName :: Item a -> Compiler String
categoryName (Item i _) = return $ getL1Category  i

createTagsPages :: Tags -> Context String -> Rules ()
createTagsPages tags baseCtx = tagsRules tags $
  \tag ptrn -> do
    let title = tag

    route indexHtmlRoute
    compile $ do
      posts <- recentFirst =<< loadAll ptrn
      let tagsCtx =
            constField "title" title
            <> listField "posts" baseCtx (return posts)
            <> constField "tag" tag
            <> baseCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

getL1Category :: Identifier -> String
getL1Category = fromMaybe "uncategorized" . headMaybe . drop 1 . splitDirectories . dropExtension . toFilePath

getL1Category' :: MonadMetadata m => Identifier -> m [String]
getL1Category' = return . return . getL1Category

--------------------------------------------------------------------------------

type URL = Text

data BlogConfig = BlogConfig
  { name          :: Text
  , handle        :: Text
  , avatar        :: URL
  , github        :: URL
  , linkedin      :: URL
  , stackoverflow :: URL
  , gpgPublicQr   :: URL
  , resume        :: URL
  , homeTitle     :: Text
  } deriving (Generic, Show)

instance Interpret BlogConfig


config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "build/site"
  , storeDirectory       = "build/_store"
  , tmpDirectory         = "build/_tmp"
  }

sassOptions :: SassOptions
sassOptions = defaultSassOptions
  { sassSourceMapEmbed = True
  , sassOutputStyle    = SassStyleCompressed
  }

blogConfigCtx :: BlogConfig -> Context String
blogConfigCtx conf =
  pickAs "homeTitle" homeTitle
  <> pickAs "github"          github
  <> pickAs "name"          name
  <> pickAs "handle"        handle
  <> pickAs "linkedin"      linkedin
  <> pickAs "stackoverflow" stackoverflow
  <> pickAs "avatar"        avatar
  <> pickAs "gpgPublicQr"   gpgPublicQr
  <> pickAs "resume"        resume
  <> defaultContext
  where
    pick a = unpack . a $ conf
    pickAs s a = constField s (pick a)

contentGlob :: Pattern
contentGlob = "content/**.md"

jpgsGlob :: Pattern
jpgsGlob = "images/**.jpg"

postCtx' :: BlogConfig -> Tags -> Tags -> Context String
postCtx' conf categories tags =  dateField "date" "%B %e, %Y"
        <> field "category" categoryName
        <> categoryField "categoryHtml" categories
        <> tagsField "tagsHtml" tags
        <> blogConfigCtx conf
        <> defaultContext


run :: IO ()
run = do
  blogConf :: BlogConfig <- input auto "./config.dhall"

  hakyllWith config $ do
    categories <- buildTagsWith getL1Category' contentGlob (fromCapture "*.html")
    tags <- buildTags contentGlob (fromCapture "tags/*.html")

    let postCtx = postCtx' blogConf categories tags

    -- compress images
    match jpgsGlob $ do
      route idRoute
      compile $ loadImage
        >>= compressJpgCompiler 50

    -- inform hakyll about templates
    match "templates/**.html" $ compile templateCompiler

    -- copy assets (non images and non post files)
    match ("content/**" .&&. complement contentGlob .&&. complement jpgsGlob) $ do
      route idRoute
      compile copyFileCompiler

    match ("assets/**" .&&. complement jpgsGlob .&&. complement "assets/css/**") $ do
      route idRoute
      compile copyFileCompiler

    -- compile SASS/CSS
    depends <- makePatternDependency "assets/css/**.scss"
    rulesExtraDependencies [depends] $ do
      let sassCompiler = sassCompilerWith sassOptions
      match (fromRegex "^assets/css/[^_].*.scss") $ do
        route $ setExtension "css"
        compile sassCompiler

    -- assemble content
    match contentGlob $ do
      let ctx = postCtx
      route $ composeRoutes indexHtmlRoute rootlessRoute
      compile $
        pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

    -- assemble tag and category pages
    createTagsPages tags postCtx
    createTagsPages categories postCtx

    -- Create index.html
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll contentGlob
        let indexCtx = listField "posts" postCtx (return $ take 5 posts)
              <> constField "isHome" "yes"
              <> blogConfigCtx blogConf
              <> defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
