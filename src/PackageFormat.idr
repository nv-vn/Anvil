module PackageFormat

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators
import Lightyear.StringFile

import Package
import ConfigProvider

%language TypeProviders
%provide (defaultDir : String) with getPkgPath

record PackageMeta where
  constructor Meta
  name, description, license, homepage, issues, maintainer : String
  contributors : List String
  installCandidates : List Pkg

instance Show PackageMeta where
  show pkg = "\n " ++ unwords
             [ "Name: " ++ name pkg ++ "\n"
             , "Description: " ++ description pkg ++ "\n"
             , "License: " ++ license pkg ++ "\n"
             , "Homepage: " ++ homepage pkg ++ "\n"
             , "Issues: " ++ issues pkg ++ "\n"
             , "Maintainer: " ++ maintainer pkg ++ "\n"
             , "Contributors: " ++ (show $ contributors pkg) ++ "\n"
             , "Install Candidates: " ++ (show $ installCandidates pkg) ++ "\n"
             ]

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

instance Show Sexpr where
  show (Ident s)    = s
  show (Literal s)  = "\"" ++ s ++ "\""
  show (ExpList xs) = show xs

parseVersion : Parser Version
parseVersion = (\[x, y, z] => V (cast x) (cast y) (cast z)) <$> sepByN 3 integer (char '.')

instance Alternative (Either String) where     -- Needed for us to use choice
  empty   = Left "Error in package definition/empty"
  a <|> b = case (a, b) of
              (Right a,      _) => Right a
              (Left _, Right a) => Right a
              (Left e, Left  _) => Left e

join : Either a (Either a b) -> Either a b
join (Left a)          = Left a
join (Right (Left a))  = Left a
join (Right (Right b)) = Right b

find : String -> Sexpr -> Either String Sexpr
find q (ExpList $ (Ident x)::xs) = if x == q then
                                     return $ ExpList $ (Ident x)::xs
                                   else choice $ map (find q) xs
find q s                         = Left $ "Error in package definition/find " ++ q ++ " for " ++ show s

extract : Sexpr -> Either String String
extract (ExpList $ _::(Literal s)::_) = return s
extract (ExpList $ _::(Ident s)::_)   = return s
extract (ExpList _)                   = Left "Error in package definition/extract"

extractl : Sexpr -> Either String (List String)
extractl (ExpList [])      = return []
extractl (ExpList $ (Literal x)::xs) = do return $ x :: !(extractl $ ExpList xs)
extractl (ExpList $ (Ident x)::xs)   = do extractl $ ExpList xs
extractl _                           = Left "Error in package definition/extractl"

getCandidates : String -> List Sexpr -> Either String (List Pkg)
getCandidates name []      = return []
getCandidates name (x::xs) = do case parseCandidate x of
                                  Left e  => Left e
                                  Right p => return $ p :: !(getCandidates name xs)
  where parseCandidate : Sexpr -> Either String Pkg
        parseCandidate sexpr = do root <- find "v" sexpr
                                  version <- parse parseVersion !(extract root) -- Finish parsing the version number...
                                  deps <- find "depends" root -- Finish building this tree...
                                  src <- extract !(find "src" root)
                                  hash <- extractHash !(find "hash" root <|> find "hash-sha1" root <|> find "hash-md5" root <|> (Right $ ExpList []))
                                  compile <- extract !(find "compile" root <|> (Right $ ExpList [])) <|> return "./configure && make"
                                  install <- extract !(find "install" root <|> (Right $ ExpList [])) <|> return "sudo make install"
                                  uninstall <- extract !(find "uninstall" root <|> (Right $ ExpList [])) <|> return "sudo make uninstall"
                                  return $ Package name version compile install uninstall src hash []
    where extractHash : Sexpr -> Either String Hash
          extractHash (ExpList [Ident "hash",      Literal h]) = return $ Blake2 h
          extractHash (ExpList [Ident "hash-md5",  Literal h]) = return $ Md5 h
          extractHash (ExpList [Ident "hash-sha1", Literal h]) = return $ Sha1 h
          extractHash _                                        = return None

getData : Sexpr -> Either String PackageMeta
getData sexpr = do root <- find "pkg" sexpr
                   name <- extract !(find "name" root)
                   desc <- extract !(find "description" root)
                   license <- extract !(find "license" root)
                   homepage <- extract !(find "homepage" root)
                   issues <- extract !(find "issues" root)
                   maintainer <- extract !(find "maintainer" root)
                   contributors <- extractl !(find "contributors" root)
                   candidates <- getCandidates name $ case !(find "candidates" root) of
                                                        ExpList (_::xs) => xs
                                                        _               => []
                   return (Meta name desc license homepage issues maintainer contributors candidates)

parseMeta : Parser (Either String PackageMeta)
parseMeta = do tree <- sexpr
               return $ getData tree

readPackage : String -> Eff (Either String PackageMeta) [FILE_IO ()]
readPackage name = do meta <- readFile (\err => "Couldn't find package: " ++ name) (defaultDir ++ "db/" ++ name ++ ".forge")
                      return $ join $ meta >>= parse parseMeta

readSexpr : String -> Eff (Either String Sexpr) [FILE_IO ()]
readSexpr name = do sexp <- readFile (\err => "Couldn't find package: " ++ name) (defaultDir ++ "db/" ++ name ++ ".forge")
                    return $ sexp >>= parse sexpr
