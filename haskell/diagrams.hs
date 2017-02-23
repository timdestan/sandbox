{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Extends f to a monoid homomorphism from [a] -> m.
hom :: Monoid m => (a -> m) -> ([a] -> m)
hom f = mconcat . map f

-- Monoid without idenity.
class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup [a] where
  (<>) = (++)

newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance (Semigroup a, Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty
  mappend = (<>)

newtype Option a = Option { getOption :: Maybe a }

none :: Option a
none = Option Nothing

some :: a -> Option a
some = Option . Just

instance Semigroup a => Monoid (Option a) where
  mempty = none
  Option Nothing `mappend` a = a
  a `mappend` Option Nothing = a
  Option (Just a) `mappend` Option (Just b) = some $ a <> b

data Pos = Pos { x :: Double, y :: Double}

instance Show Pos where
  show Pos { x, y } = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

data Prim = Circle { center :: Pos, radius :: Double }
  deriving (Show)

newtype Diagram = Diagram (Dual [Prim])
  deriving (Semigroup, Monoid)

unD :: Diagram -> [Prim]
unD (Diagram (Dual ps)) = ps

prim :: Prim -> Diagram
prim p = Diagram (Dual [p])

mkD :: [Prim] -> Diagram
mkD ps = Diagram (Dual ps)

main =
  putStrLn $ show Circle { center = Pos { x = 4, y = 3}, radius = 7 }
