module ConfigProvider

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.StringFile

import Prelude.Providers
%language TypeProviders

record Field where
  constructor MkField
  fname, fvalue : String

parseFields : Parser (List Field)
parseFields = many parseField
  where parseField : Parser Field
        parseField = do name <- many letter
                        spaces *> colon <* spaces
                        val <- quoted '"'
                        many newline
                        return $ MkField (pack name) val

getConfig : IO (Either String $ List Field)
getConfig = do Right file <- readFile "../config.cfg"
                     | Left err => return (Left "Couldn't read config file, using defaults!")
               return $ parse parseFields file

getConfigField : String -> String -> IO (Provider String)
getConfigField field d = do case !getConfig of
                              Left err  => do putStrLn $ show err
                                              return $ pure d
                              Right cfg => case filter (\i => fname i == field) cfg of
                                             []   => return $ pure d
                                             x::_ => return $ pure $ fvalue x

getPkgPath : IO (Provider String)
getPkgPath = getConfigField "defaultPkgPath" "./example/"

getPrimarySrc : IO (Provider String)
getPrimarySrc = getConfigField "primarySources" "db/"

getOverlays : IO (Provider String)
getOverlays = getConfigField "overlays" ".overlays"
