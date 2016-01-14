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
  Between : Version -> Version -> PkgConstraint

instance Show PkgConstraint where
  show Any           = ""
  show (Up v)        = "(>= " ++ show v ++ ")"
  show (Down v)      = "(<= " ++ show v ++ ")"
  show (Between v w) = "(>= " ++ show v ++ ", <=" ++ show w ++ ")"

data Pkg : Type where
  Package : String -> Version -> List Pkg -> Pkg -- Change to List Dep

data Dep : Type where
  Dependency : String -> PkgConstraint -> List Dep -> Dep

data InstallResult = Installed (List Pkg)
                   | NotReady
                   | Error String

installable : Pkg -> Bool
installable (Package _ _ []) = True
installable (Package _ _ xs) = False

installable' : Pkg -> PkgConstraint -> Bool
installable' (Package _ _ []) Any = True
installable' (Package _ v []) (Up w) = v >= w
installable' (Package _ v []) (Down w) = v <= w
installable' (Package _ v []) (Between w x) = w <= v && v <= x
installable' (Package _ _ xs) _ = False

instance Eq Pkg where
  (Package s v _) == (Package s' v' _) = s == s' && v == v'

instance Ord Pkg where
  compare (Package s v _) (Package s' v' _) = if s > s' then GT else
                                              if s < s' then LT else
                                              if v > v' then GT else
                                              if v < v' then LT else EQ

instance Show Pkg where
  show (Package s v xs) = "Package " ++ s ++ " " ++ show v

instance [verbose] Show Pkg where
  show (Package s v xs) = "Package " ++ s ++ " " ++ show v ++ " depends on: " ++ show xs

install : (p : Pkg) -> {auto q : installable p = True} -> Either (IO ()) String
install (Package s v []) = Left $ do putStrLn $ "Cloning package " ++ s
                                     putStrLn $ "Compiling package " ++ s
                                     putStrLn $ "Installing package " ++ s

handle : Either (IO ()) String -> List Pkg -> IO InstallResult
handle (Right msg) pkgs = return $ Error msg
handle (Left eff)  pkgs = do eff
                             putStrLn "Done."
                             return $ Installed pkgs

prepare : Pkg -> List Pkg -> IO InstallResult
prepare (Package s v [])      pkgs = if (Package s v []) `elem` pkgs then return $ Installed pkgs
                                     else handle (install $ Package s v []) $ (Package s v []) :: pkgs
prepare (Package s v $ x::xs) pkgs = if x `elem` pkgs then prepare (Package s v xs) pkgs
                                     else do case !(prepare x pkgs) of
                                               Installed pkgs' => prepare (Package s v xs) pkgs'
                                               Error msg       => return $ Error msg
                                               NotReady        => prepare (Package s v $ xs ++ [x]) pkgs

main : IO ()
main = do let packages = []
          let H = Package "H" (V 3 5 1) []
          let D = Package "D" (V 1 6 7)
                    [ Package "F" (V 2 4 0) [H]
                    , Package "G" (V 0 3 7) [H]
                    ]
          let C = Package "C" (V 2 4 3) [D]
          let B = Package "B" (V 1 4 1)
                    [ Package "E" (V 3 2 1) []
                    , C
                    ]
          let A = Package "A" (V 1 2 3) [B, C, D]
          case !(prepare A packages) of
            Installed pkgs => putStrLn $ "Successfully installed packages:" ++ show pkgs ++ "."
            Error msg      => putStrLn $ "Encountered error: '" ++ msg ++ "' while installing packages."
            NotReady       => putStrLn $ "Could not complete the request at this time. Packages may be broken."
