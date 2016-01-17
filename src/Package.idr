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

data Pkg : Type where
  Package : String -> Version -> List Dep -> Pkg -- Should packages still store their dependencies?

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
