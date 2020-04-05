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
import           Text.Sass.Options (SassOptions (..), SassOutputStyle (..),
                                    defaultSassOptions)
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
  } deriving (Generic, Show)

instance Interpret BlogConfig


config :: Configuration
config = defaultConfiguration { destinationDirectory = "build/site"
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
  pickAs "github" github
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

postsGlob :: Pattern
postsGlob = "content/**.md"

jpgsGlob :: Pattern
jpgsGlob = "images/**.jpg"

postCtx :: Context String
postCtx =  dateField "date" "%B %e, %Y"
        <> constField "item-type" "post"
        <> pathField "sourcefile"
        <> defaultContext

run :: IO ()
run = do
  blogConf :: BlogConfig <- input auto "./config.dhall"

  hakyllWith config $ do
    -- compress images
    match jpgsGlob $ do
      route idRoute
      compile $ loadImage
        >>= compressJpgCompiler 50

    -- copy assets (non images and non post files)
    match ("content/**" .&&. complement postsGlob .&&. complement jpgsGlob) $ do
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

    -- Create index.html
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll postsGlob
        let indexCtx = listField "posts" postCtx (return posts)
              <> constField "title" "Home"
              <> blogConfigCtx blogConf
              <> defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= relativizeUrls

--------------------------------------------------------------------------------
