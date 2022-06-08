--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import qualified Text.Pandoc as Pandoc
import qualified GHC.IO.Encoding as E
import Control.Applicative (empty)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import qualified Data.Text as T

--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do
    forM_
      [ "CNAME"
      , "favicon.ico"
      , "robots.txt"
      , "_config.yml"
      , "images/*"
      , "js/*"
      , "fonts/*"
      , "exercises/*"
      ]
      $ \f -> match f $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["preface.org"]) $ do
      route $ (gsubRoute "preface.org" (const "index")) `composeRoutes` (setExtension "html")
      compile $ do
        lessons <- loadAll "lessons/*.org"
        let tocContext =
              field "lessonNumber" lessonNumber <>
              defaultContext
        let prefaceContext =
              listField "toc" tocContext (return lessons) <>
              field "firstLessonUrl" firstLessonUrl <>
              field "firstLessonTitle" firstLessonTitle <>
              constField "preface" "true" <>
              defaultContext
        pandocCompiler
          >>= loadAndApplyTemplate "templates/lesson.html" prefaceContext
          >>= loadAndApplyTemplate "templates/default.html" prefaceContext
          >>= relativizeUrls

    match "lessons/*" $ do
      route $ setExtension "html"
      compile $ do
        let lessonContext =
              field "lessonNumber" lessonNumber <>
              field "nextLessonUrl" nextLessonUrl <>
              field "nextLessonTitle" nextLessonTitle <>
              field "prevLessonUrl" prevLessonUrl <>
              field "prevLessonTitle" prevLessonTitle <>
              defaultContext
        pandocCompilerWith defaultHakyllReaderOptions withTOC
          >>= loadAndApplyTemplate "templates/lesson.html"    lessonContext
          >>= loadAndApplyTemplate "templates/default.html" lessonContext
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "dist"
  , previewHost = "127.0.0.1"
  , previewPort = 8000
  , providerDirectory = "src"
  , storeDirectory = "ssg/_cache"
  , tmpDirectory = "ssg/_tmp"
  }

withTOC :: Pandoc.WriterOptions
withTOC = defaultHakyllWriterOptions
  { Pandoc.writerNumberSections = False
  , Pandoc.writerTableOfContents = True
  , Pandoc.writerTOCDepth = 2
  , Pandoc.writerTemplate = Just tocTemplate
  }
  where
    tocTemplate =
      either error id $ either (error . show) id
      $ Pandoc.runPure $ Pandoc.runWithDefaultPartials
      $ Pandoc.compileTemplate ""
      $ T.unlines
      [ "<nav id=\"toc\" class=\"card\">"
      , "<h1>Contents</h1>"
      , "$toc$"
      , "</nav>"
      , "<article class=\"card\"><section id=\"lesson-body\">$body$</section>"
      ]

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
  -- TODO: use System.FilePath.Posix functions
  if "lessons" `isPrefixOf` path
    then return . numPrefix . drop (length ("lessons" :: String) + 1) $ path
    else empty
  where
    numPrefix = dropWhile (== '0') . take 2

nextLessonUrl :: Item String -> Compiler String
nextLessonUrl lesson = do
  ident <- nextLessonIdentifier lesson
  case ident of
    Just i -> (fmap (maybe empty toUrl) . getRoute) i
    Nothing -> empty

nextLessonTitle :: Item String -> Compiler String
nextLessonTitle lesson = do
  ident <- nextLessonIdentifier lesson
  case ident of
    Just i -> do
      getMetadataField i "title"
      >>= return . fromMaybe "No title"
    Nothing -> empty

prevLessonUrl :: Item String -> Compiler String
prevLessonUrl lesson = do
  ident <- prevLessonIdentifier lesson
  case ident of
    Just i -> (fmap (maybe empty toUrl) . getRoute) i
    Nothing -> empty

prevLessonTitle :: Item String -> Compiler String
prevLessonTitle lesson = do
  ident <- prevLessonIdentifier lesson
  case ident of
    Just i -> do
      getMetadataField i "title"
      >>= return . fromMaybe "No title"
    Nothing -> empty

getLessons :: MonadMetadata m => m [Identifier]
getLessons = getMatches "lessons/*.org"

nextLessonIdentifier :: MonadMetadata m => Item String -> m (Maybe Identifier)
nextLessonIdentifier lesson = do
  lessons <- getLessons
  return $ itemAfter lessons $ itemIdentifier lesson
  where
    itemAfter :: Eq a => [a] -> a -> Maybe a
    itemAfter xs x = lookup x $ zip xs (tail xs)

prevLessonIdentifier :: MonadMetadata m => Item String -> m (Maybe Identifier)
prevLessonIdentifier lesson = do
  lessons <- getLessons
  return $ itemBefore lessons $ itemIdentifier lesson
  where
    itemBefore :: Eq a => [a] -> a -> Maybe a
    itemBefore xs x = lookup x $ zip (tail xs) xs
