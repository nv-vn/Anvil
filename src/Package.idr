module Package

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

data Hash = None
          | Md5 String
          | Sha1 String
          | Blake2 String

record Pkg where
  constructor Package
  pname : String
  version : Version
  compile, install, uninstall : String
  src : String
  hash : Hash
  deps : List Dep -- Should packages still store their dependencies?

instance Eq Pkg where
  p == p' = pname p == pname p' && version p == version p'

instance Ord Pkg where
  compare p p' = if pname p > pname p' then GT else
                 if pname p < pname p' then LT else
                 if version p > version p' then GT else
                 if version p < version p' then LT else EQ

instance Show Pkg where
  show p = "Package " ++ pname p ++ " " ++ (show $ version p)

queryPkg : String -> PkgConstraint -> List Pkg -> Maybe Pkg
queryPkg query constraint srcs = head' matches
  where matches = filter (\pkg => pname pkg == query && inRange (version pkg) constraint) $ reverse $ sort srcs -- This is slow, will bottleneck
