--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import qualified GHC.IO.Encoding as E
import Control.Applicative (empty)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)


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

    match (fromList ["CNAME"]) $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["preface.org"]) $ do
      route $ (gsubRoute "preface.org" (const "index")) `composeRoutes` (setExtension "html")
      compile $ do
        let prefaceContext =
              field "firstLessonUrl" firstLessonUrl <>
              field "firstLessonTitle" firstLessonTitle <>
              defaultContext
        pandocCompiler
          >>= loadAndApplyTemplate "templates/lesson.html" prefaceContext
          >>= loadAndApplyTemplate "templates/default.html" prefaceContext
          >>= relativizeUrls

    match "lessons/*" $ do
      route $ setExtension "html"
      compile $ do
        let lessonContext =
              field "nextLessonUrl" nextLessonUrl <>
              field "nextLessonTitle" nextLessonTitle <>
              field "lessonNumber" lessonNumber <>
              defaultContext
        pandocCompiler
          >>= loadAndApplyTemplate "templates/lesson.html"    lessonContext
          >>= loadAndApplyTemplate "templates/default.html" lessonContext
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

firstLessonUrl :: Item String -> Compiler String
firstLessonUrl _ = do
  lessons <- getMatches "lessons/*.org"
  fmap (maybe empty toUrl) . getRoute $ head lessons

firstLessonTitle :: Item String -> Compiler String
firstLessonTitle _ = do
  lessons <- getMatches "lessons/*.org"
  getMetadataField (head lessons) "title"
    >>= return . fromMaybe "No title"

lessonNumber :: Item String -> Compiler String
lessonNumber lesson = do
  let path = toFilePath (itemIdentifier lesson)
  if "lessons/" `isPrefixOf` path
    then return . take 2 . drop (length ("lessons/" :: String)) $ path
    else empty

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
