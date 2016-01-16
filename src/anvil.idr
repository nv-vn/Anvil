module Main

data Version : Type where
  V : (major : Nat) -> (minor : Nat) -> (patch : Nat) -> Version

instance Eq Version where
  (V a b c) == (V a' b' c') = (a == a') && (b == b') && (c == c')

instance Ord Version where
  compare (V a b c) (V a' b' c') = if a > a' then GT else
                                   if a == a' && b > b' then GT else
                                   if a == a' && b == b' && c > c' then GT else
                                   if a == a' && b == b' && c == c' then EQ else LT

instance Show Version where
  show (V a b c) = "version " ++ show a ++ "." ++ show b ++ "." ++ show c

data PkgConstraint : Type where
  Any     : PkgConstraint
  Up      : Version -> PkgConstraint
  Down    : Version -> PkgConstraint
  Exactly : Version -> PkgConstraint
  Between : Version -> Version -> PkgConstraint

instance Show PkgConstraint where
  show Any           = ""
  show (Up v)        = "(>= " ++ show v ++ ")"
  show (Down v)      = "(<= " ++ show v ++ ")"
  show (Exactly v)   = "(= " ++ show v ++ ")"
  show (Between v w) = "(>= " ++ show v ++ ", <=" ++ show w ++ ")"

inRange : Version -> PkgConstraint -> Bool
inRange v Any           = True
inRange v (Up w)        = v >= w
inRange v (Down w)      = v <= w
inRange v (Exactly w)   = v == w
inRange v (Between w x) = w <= v && v <= x

data Dep : Type where
  Dependency : String -> PkgConstraint -> List Dep -> Dep

instance Show Dep where
  show (Dependency s v xs) = "Package " ++ s ++ " " ++ show v

instance [verbose] Show Dep where
  show (Dependency s v xs) = "Package " ++ s ++ " " ++ show v ++ " depends on: " ++ show xs

data Pkg : Type where
  Package : String -> Version -> List Dep -> Pkg -- Should packages still store their dependencies?

data InstallResult = Installed (List Pkg)
                   | NotReady
                   | Error String

installable : Pkg -> Bool
installable (Package _ v []) = True
installable (Package _ _ xs) = False

instance Eq Pkg where
  (Package s v _) == (Package s' v' _) = s == s' && v == v'

instance Ord Pkg where
  compare (Package s v _) (Package s' v' _) = if s > s' then GT else
                                              if s < s' then LT else
                                              if v > v' then GT else
                                              if v < v' then LT else EQ

instance Show Pkg where
  show (Package s v xs) = "Package " ++ s ++ " " ++ show v

queryPkg : String -> PkgConstraint -> List Pkg -> Maybe Pkg
queryPkg name constraint srcs = head' matches
  where matches = filter (\(Package n v _) => n == name && inRange v constraint) $ reverse $ sort srcs -- This is slow, will bottleneck

--| TODO: Require that the Pkg be proven to be in the range of a PkgConstraint
install : (p : Pkg) -> {auto q : installable p = True} -> Either String (IO ())
install (Package s v []) = Right $ do putStrLn $ "Cloning package " ++ s
                                      putStrLn $ "Compiling package " ++ s
                                      putStrLn $ "Installing package " ++ s

handle : Either String (IO ()) -> List Pkg -> IO InstallResult
handle (Left msg)  pkgs = return $ Error msg
handle (Right eff) pkgs = do eff
                             putStrLn "Done."
                             return $ Installed pkgs

prepare : Dep -> List Pkg -> List Pkg -> IO InstallResult
prepare (Dependency s c [])      pkgs srcs = case queryPkg s c srcs of
                                               Just (Package s v []) => let pkg = Package s v [] in
                                                                        handle (install pkg) (pkg :: pkgs)
                                               Nothing  => return $ Error $ "No package matching " ++ show (Dependency s c []) ++ " is available"
prepare (Dependency s c $ x::xs) pkgs srcs = case queryPkg' x srcs of
                                               Nothing  => return $ Error $ "No package matching " ++ show x ++ " is available"
                                               Just pkg => if pkg `elem` pkgs then prepare (Dependency s c xs) pkgs srcs
                                                           else do case !(prepare x pkgs srcs) of
                                                                     Installed pkgs' => prepare (Dependency s c xs) pkgs' srcs
                                                                     Error msg       => return $ Error msg
                                                                     NotReady        => prepare (Dependency s c $ xs ++ [x]) pkgs srcs
  where queryPkg' : Dep -> List Pkg -> Maybe Pkg
        queryPkg' (Dependency s v _) = queryPkg s v

main : IO ()
main = do let packages = [] -- Represents installed packages on system
          let H = Dependency "H" (Up $ V 3 5 0) []
          let D = Dependency "D" (Down $ V 1 7 0)
                    [ Dependency "F" (Exactly $ V 2 4 0) [H]
                    , Dependency "G" (Between (V 0 0 0)  (V 1 0 0)) [H]
                    ]
          let C = Dependency "C" Any [D]
          let B = Dependency "B" (Exactly $ V 1 4 1)
                    [ Dependency "E" (Exactly $ V 3 2 1) []
                    , C
                    ]
          let A = Dependency "A" (Exactly $ V 1 2 3) [B, C, D]
          let sources = [Package "H" (V 3 5 1) [], -- Represents database of available packages
                         Package "H" (V 3 5 0) [],
                         Package "D" (V 1 6 7) [],
                         Package "F" (V 2 4 0) [],
                         Package "G" (V 0 3 7) [],
                         Package "C" (V 2 4 3) [],
                         Package "B" (V 1 4 1) [],
                         Package "E" (V 3 2 1) [],
                         Package "A" (V 1 2 3) []]
          case !(prepare A packages sources) of
            Installed pkgs => putStrLn $ "Successfully installed packages:" ++ show pkgs ++ "."
            Error msg      => putStrLn $ "Encountered error: '" ++ msg ++ "' while installing packages."
            NotReady       => putStrLn $ "Could not complete the request at this time. Packages may be broken."
