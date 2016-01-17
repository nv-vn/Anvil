module PackageFormat

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators

import Package

record PackageMeta where
  constructor Meta
  name, description, license, homepage, issues, maintainer : String
  contributors : List String
  installCandidates : List Pkg
  build, install, uninstall : String -- Maybe custom datatype for commands? Or should we have dedicated install scripts?

data Sexpr = Ident   String   -- Should we add dotted pairs?
           | Literal String
           | ExpList (List Sexpr)

spacedOut : Parser a -> Parser a
spacedOut x = spaces *> x <* spaces

ident : Parser Sexpr
ident = Ident <$> liftA2 strCons first (the (List Char -> String) pack <$> rest)
  where sym : Parser Char -- Why is this one needed, but not the others?
        sym   = oneOf "!@#$%^&*-_=+[{]};:'\\|,<.>/?`~"
        first = letter <|> sym
        rest  = many $ alphaNum <|> sym

literal : Parser Sexpr
literal = Literal <$> quoted '"'

mutual
  explist : Parser Sexpr
  explist = ExpList <$> (char '(') >! many sexpr <* (char ')')

  sexpr : Parser Sexpr
  sexpr = spacedOut ident <|> spacedOut literal <|> spacedOut explist

instance Alternative (Either String) where     -- Needed for us to use choice
  empty   = Left "Error in package definition"
  a <|> b = case (a, b) of
              (Right a,      _) => Right a
              (Left _, Right a) => Right a
              (Left _, Left  _) => empty

join : Either a (Either a b) -> Either a b
join (Left a)          = Left a
join (Right (Left a))  = Left a
join (Right (Right b)) = Right b

getData : Sexpr -> Either String PackageMeta
getData sexpr = do root <- find "pkg" sexpr
                   return (Meta "a" "b" "c" "d" "e" "f" ["g"] [] "h" "i" "j")
  where find : String -> Sexpr -> Either String Sexpr
        find q (ExpList $ (Literal x)::xs) = if x == q then
                                               return $ ExpList $ (Literal x)::xs
                                             else choice $ map (find q) xs
        find _ _                           = Left "Error in package definition"

parseMeta : Parser (Either String PackageMeta)
parseMeta = do tree <- sexpr
               return $ getData tree

defaultDir : String
defaultDir = "./example/" -- Should get this from a type provider?

readPackage : String -> Eff (Either String PackageMeta) [FILE_IO ()]
readPackage name = do meta <- readFile (\err => "Couldn't find package: " ++ name) (defaultDir ++ "db/" ++ name ++ ".forge")
                      return $ join $ meta >>= parse parseMeta
