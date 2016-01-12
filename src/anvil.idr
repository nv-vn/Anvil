module Main

data Version :  Type where
  V : (major : Nat) -> (minor : Nat) -> (patch : Nat) -> Version

instance Eq Version where
  (V a b c) == (V a' b' c') = (a == a') && (b == b') && (c == c')

instance Ord Version where
  compare (V a b c) (V a' b' c') = if a > a' then GT else
                                   if a == a' && b > b' then GT else
                                   if a == a' && b == b' && c > c' then GT else
                                   if a == a' && b == b' && c == c' then EQ else LT

data Pkg : Type where
  Package : String -> Version -> List Pkg -> Pkg

data InstallResult = Installed
                   | NotReady
                   | Error String

installable : Pkg -> Bool
installable (Package _ _ []) = True
installable (Package _ _ xs) = False

instance Eq Pkg where
  (Package s v _) == (Package s' v' _) = s == s' && v == v'

install : (p : Pkg) -> {auto q : installable p = True} -> Either (IO ()) String
install (Package s v []) = Left $ do putStrLn $ "Cloning package " ++ s
                                     putStrLn $ "Compiling package " ++ s
                                     putStrLn $ "Installing package " ++ s

handle : Either (IO ()) String -> IO InstallResult
handle (Right msg) = return $ Error msg
handle (Left eff)  = do eff
                        putStrLn "Done."
                        return Installed

prepare : Pkg -> IO InstallResult
prepare (Package s v [])      = handle $ install (Package s v []) 
prepare (Package s v $ x::xs) = do result <- prepare x
                                   case result of
                                     Installed => prepare (Package s v xs)
                                     Error msg => return $ Error msg
                                     NotReady  => prepare (Package s v $ xs ++ [x])

main : IO ()
main = do let x = Package "A" (V 1 2 3) [Package "B" (V 1 4 1) [], Package "C" (V 2 5 3) []] 
          result <- prepare x
          case result of
            Installed => putStrLn $ "Successfully installed packages."
            Error msg => putStrLn $ "Encountered error: '" ++ msg ++ "' while installing packages."
            NotReady  => putStrLn $ "Could not complete the request at this time. Packages may be broken."
