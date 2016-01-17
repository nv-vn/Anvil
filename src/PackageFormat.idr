module PackageFormat

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Package

record PackageMeta where
  constructor Meta
  name, description, license, homepage, issues, maintainer : String
  contributors : List String
  installCandidates : List Pkg
  build, install, uninstall : String -- Maybe custom datatype for commands? Or should we have dedicated install scripts?

parseMeta : Parser PackageMeta
parseMeta = ?some_parser

defaultDir : String
defaultDir = "~/.anvil/" -- Should get this from a type provider?

readPackage : String -> Eff (Either String PackageMeta) [FILE_IO ()]
readPackage name = do meta <- readFile (\err => "Couldn't find package: " ++ name) (defaultDir ++ "db/" ++ name)
                      return $ meta >>= parse parseMeta
