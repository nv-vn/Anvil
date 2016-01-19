module ConfigProvider

import Effects
import Effect.File

import Lightyear
import Lightyear.Char
import Lightyear.Strings

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

getConfig : Eff (Either String $ List Field) [FILE_IO ()]
getConfig = do file <- readFile (\err => "Couldn't read config! Using defaults.") "../config.cfg"
               return $ file >>= parse parseFields

getPkgPath : IO (Provider String)
getPkgPath = do case !(run getConfig) of
                  Left err  => do putStrLn err
                                  return $ Provide "./example/"
                  Right cfg => case filter (\i => fname i == "defaultPkgPath") cfg of
                                 []   => return $ Provide "./example/"
                                 x::_ => return $ Provide $ fvalue x
