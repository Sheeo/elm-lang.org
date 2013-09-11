{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Aeson
import System.Exit
import System.FilePath
import System.Directory
import Text.Parsec
import qualified Language.Elm as Elm

main = do
  json <- BS.readFile =<< Elm.docs
  case eitherDecode json of
    Left err -> print err >> exitFailure
    Right docs ->
        case Either.partitionEithers (map docToElm docs) of
          ([], elms) -> mapM writeDocs elms
          (errs, _) -> mapM putStrLn errs >> exitFailure

writeDocs (name, code) =
  do putStrLn name
     createDirectoryIfMissing True dir
     writeFile fileName code
  where
    fileName =  dir </> last fileParts <.> "elm"

    dir = ".." </> "public" </> "docs" </> joinPath (init fileParts)
    fileParts = split name

    split [] = []
    split xs = hd : split (dropWhile (=='.') tl)
        where (hd,tl) = span (/='.') xs

data Document = Doc
    { moduleName :: String
    , structure :: String
    , entries :: [Entry]
    } deriving (Show)

data Entry = Entry
    { name :: String
    , comment :: String
    , raw :: String
    , assocPrec :: Maybe (String,Int)
    } deriving (Show)

instance FromJSON Document where
    parseJSON (Object v) =
        Doc <$> v .: "name"
            <*> v .: "document"
            <*> (concat <$> sequence [ v .: "aliases", v .: "datatypes", v .: "values" ])

    parseJSON _ = fail "Conversion to Document was expecting an object"

instance FromJSON Entry where
    parseJSON (Object v) =
        Entry <$> v .: "name"
              <*> v .: "comment"
              <*> v .: "raw"
              <*> (liftM2 (,) <$> v .:? "associativity"
                              <*> v .:? "precedence")

    parseJSON _ = fail "Conversion to Entry was expecting an object"

data Content = Markdown String | Value String
               deriving Show

docToElm doc =
  let name = moduleName doc
      entries = getEntries doc
  in
  do contents <- either (Left . show) Right $ parse (parseDoc []) name (structure doc)
     case Either.partitionEithers $ map (contentToElm (getEntries doc)) contents of
       ([], code) ->
           Right . (,) name $
           unlines [ "import open Docs"
                   , "import Search"
                   , "import Window"
                   , ""
                   , "main = documentation " ++ show name ++ " entries <~ Window.dimensions ~ Search.box ~ Search.results"
                   , ""
                   , "entries ="
                   , "  [ " ++ List.intercalate "\n  , " code ++ "\n  ]"
                   ]
       (missing, _) ->
           Left $ "In module " ++ name ++ ", could not find documentation for: " ++ List.intercalate ", " missing
  where
    parseDoc contents =
        choice [ eof >> return contents
               , do try (string "@docs")
                    whitespace
                    values <- sepBy1 (var <|> op) comma
                    parseDoc (contents ++ map Value values)
               , do let stop = eof <|> try (string "@docs" >> return ())
                    md <- manyTill anyChar (lookAhead stop)
                    parseDoc (contents ++ [Markdown md])
               ]

    var = (:) <$> letter <*> many (alphaNum <|> oneOf "_'")
    op = do char '(' >> whitespace
            operator <- many1 (satisfy Char.isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
            whitespace >> char ')'
            return operator

    comma = try (whitespace >> char ',') >> whitespace
    whitespace = many (satisfy (`elem` " \n\r"))

getEntries :: Document -> Map.Map String Entry
getEntries doc =
    Map.fromList $ map (\entry -> (name entry, entry)) (entries doc)

contentToElm :: Map.Map String Entry -> Content -> Either String String
contentToElm entries content =
    case content of
      Markdown md -> Right $ "flip width [markdown|<style>p,ul,pre {color:#747679;} h1 {font-weight:normal;font-size:24px;}</style>" ++ md ++ "|]"
      Value name ->
          case Map.lookup name entries of
            Nothing -> Left name
            Just entry ->
                Right $ unwords [ "entry"
                                , show name
                                , show (raw entry)
                                , case assocPrec entry of
                                    Nothing -> "Nothing"
                                    Just ap -> "(" ++ show (Just ap) ++ ")"
                                , "[markdown|" ++ comment entry ++ "|]"
                                ]
