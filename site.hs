--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import qualified GHC.IO.Encoding as E
import Control.Applicative (empty)
import Data.Maybe (fromMaybe)


--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "exercises/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["preface.org"]) $ do
      route $ (gsubRoute "preface.org" (const "index")) `composeRoutes` (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "lessons/*" $ do
        route $ setExtension "html"
        compile $ do
          let lessonContext =
                field "nextLessonUrl" nextLessonUrl <>
                field "nextLessonTitle" nextLessonTitle <>
                defaultContext
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    lessonContext
            >>= loadAndApplyTemplate "templates/default.html" lessonContext
            >>= relativizeUrls

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         lessons <- loadAll "lessons/*"
    --         let indexCtx =
    --                 listField "lessons" defaultContext (return lessons) <>
    --                 constField "title" "Preface"                <>
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

nextLessonUrl :: Item String -> Compiler String
nextLessonUrl lesson = do
  lessons <- getMatches "lessons/*.org"
  let ident = itemAfter lessons $ itemIdentifier lesson
  case ident of
    Just i -> (fmap (maybe empty toUrl) . getRoute) i
    Nothing -> empty

nextLessonTitle :: Item String -> Compiler String
nextLessonTitle lesson = do
  lessons <- getMatches "lessons/*.org"
  let ident = itemAfter lessons $ itemIdentifier lesson
  case ident of
    Just i -> do
      metadata <- getMetadata i
      return $ fromMaybe "No title" $ lookupString "title" metadata
    Nothing -> empty

itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x = lookup x $ zip xs (tail xs)
