{-# LANGUAGE LambdaCase #-}

module MyLib ( parseFile
) where

import Control.Applicative
import Data.Char (isDigit, isSpace, isHexDigit, chr)
import Data.HashMap.Strict (HashMap, fromList, keys, toList)
import Control.Monad (replicateM)
import Numeric (readHex)

data JsonValue = JsonNull
               | JsonBool   Bool
               | JsonInt    Int
            --    | JsonFloat  Float
               | JsonString String
               | JsonArray  [JsonValue]
               | JsonObject (HashMap String JsonValue)
               deriving (Eq)


newtype Parser a = Parser
                 { runParser :: String -> Maybe (a, String)}


type JsonP = Parser JsonValue

instance Show JsonValue where
  show = toJson


instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (a, str) <- p input
        return (f a, str)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  (<*>) (Parser f) (Parser a) = Parser $ \input -> do
      (res1, input')  <- f input
      (res2, input'') <- a input'
      return (res1 res2, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|> p2 input


charP :: Char -> Parser Char
charP a = Parser f
    where
        f [] = Nothing
        f (x:xs) = if x == a then Just (x, xs) else Nothing

stringP :: String -> Parser String
stringP = traverse charP

escapeP :: Parser Char
escapeP = ('"'  <$ stringP "\\\"") <|>
          ('\\' <$ stringP "\\\\") <|>
          ('/'  <$ stringP "\\/" ) <|>
          ('\b' <$ stringP "\\b" ) <|>
          ('\f' <$ stringP "\\f" ) <|>
          ('\n' <$ stringP "\\n" ) <|>
          ('\r' <$ stringP "\\r" ) <|>
          ('\t' <$ stringP "\\t" ) <|>
          (stringP "\\u" *> escapeUni )

escapeUni :: Parser Char
escapeUni = chr . fst . head . readHex <$> replicateM 4 (parseIf isHexDigit)

normalP :: Parser Char
normalP = parseIf (`notElem` "\\\"")

wsp :: Parser String
wsp = spanP isSpace

spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf


parseIf :: (Char -> Bool) -> Parser Char
parseIf pred = Parser $ \case
    c:cs | pred c  -> Just (c, cs)
    _           -> Nothing

jsonNull :: JsonP
jsonNull = JsonNull <$ stringP "null"

jsonBool :: JsonP
jsonBool = (JsonBool True <$ stringP "true") <|> (JsonBool False <$ stringP "false")





jsonInt  :: JsonP
jsonInt  = JsonInt . read <$> isParsed (spanP isDigit)
    where
        isParsed :: Parser String -> Parser String
        isParsed (Parser a) = Parser $ \input -> do
            (res1, input') <- a input
            if null res1 then Nothing else return (res1, input')


spanString :: Parser String
spanString = charP '"' *> many (normalP <|> escapeP) <* charP '"'

jsonString :: JsonP
jsonString = JsonString <$> spanString

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep val = (:) <$> val <*> many (sep *> val) <|> pure []

sepComma :: Parser Char
sepComma = wsp *> charP ',' <* wsp


jsonArray  :: JsonP
jsonArray  = JsonArray <$> (charP '[' *> wsp *> sepBy sepComma jsonParser <* wsp <* charP ']')

jsonObject :: JsonP
jsonObject = JsonObject <$> (charP '{' *> wsp *> elemMap <* wsp <* charP '}')
    where
        elemMap = fromList <$> sepBy sepComma element
        element =
            (\k _ v -> (k, v))
            <$> spanString
            <*> (wsp *> charP ':' <* wsp)
            <*> jsonParser

jsonParser :: JsonP
jsonParser = jsonNull <|> jsonBool <|> jsonInt <|> jsonString <|> jsonArray <|> jsonObject

fromJson :: String -> Maybe JsonValue
fromJson input = fst <$> runParser jsonParser input


fromPair :: (String, String) -> String
fromPair (a, b) = a ++ b

join :: String -> [String] -> String
-- join delim (x:xs) = x ++ foldl (\y -> (++) (delim++y)) "" xs
join delim (x:xs) = x ++ concatMap fromPair ([(delim, y) | y <- xs])
join _ [] = ""

showPair :: (String, JsonValue) -> String
showPair (key, value) = escape key ++ ": " ++ toJson value

showMap :: HashMap String JsonValue -> String
showMap m =  "{\n" ++ join ",\n" (map showPair pairs) ++ "\n}"
    where
        pairs = toList m


toJson :: JsonValue -> String
toJson JsonNull = "null"
toJson (JsonBool x)
    | x = "true"
    | otherwise = "false"
toJson (JsonInt x) = show x
toJson (JsonString s) = escape s
toJson (JsonArray xs) = "[\n" ++ join ",\n" (map toJson xs) ++ "\n]"
toJson (JsonObject m) = showMap m

escape :: String -> String
escape = show

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
    input <- readFile path
    return (fst <$> runParser parser input)






-- intToFrac :: Int -> Float
-- intToFrac x = fromIntegral x / (10^n)
--     where
--         n = fromIntegral . length . show $ x

-- pairToFloat :: (Int, Int) -> Float
-- pairToFloat (a, b) = fromIntegral a + intToFrac b

-- toPair :: a -> b -> (a, b)
-- toPair x y = (x, y)


-- jsonFloat :: JsonP
-- jsonFloat = JsonFloat . pairToFloat $ toPair <$> jsonInt