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

newtype Max a = Max { getMax :: a }
  deriving (Eq, Ord)

instance Ord a => Semigroup (Max a) where
  (<>) = max

instance Semigroup r => Semigroup (a -> r) where
  (f <> g) a = (f a) <> (g a)

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

instance Semigroup a => Semigroup (Option a) where
  Option Nothing <> a = a
  a <> Option Nothing = a
  Option (Just a) <> Option (Just b) = some $ a <> b

instance Semigroup a => Monoid (Option a) where
  mempty = none
  mappend = (<>)

-- 2D point
data P2 = P2 Double Double
  deriving (Eq, Show, Ord)

-- 2D vector
data V2 = V2 P2 P2
  deriving (Eq, Show, Ord)

data Prim = Circle P2 Double
  deriving (Eq, Show)

newtype Diagram = Diagram (Dual [Prim])
  deriving (Semigroup, Monoid)

instance Show Diagram where
  show (Diagram (Dual ps)) = show $ reverse ps

unD :: Diagram -> [Prim]
unD (Diagram (Dual ps)) = ps

prim :: Prim -> Diagram
prim p = Diagram (Dual [p])

mkD :: [Prim] -> Diagram
mkD ps = Diagram (Dual ps)

newtype Envelope = Envelope (Option(V2 -> Max Double))
  deriving (Semigroup, Monoid)

envelopeP :: Prim -> Envelope
envelopeP = undefined

translateP :: V2 -> Prim -> Prim
translateP (V2 (P2 x0 y0) (P2 x1 y1))
           (Circle (P2 xc yc) r) = Circle (P2 (xc + x1 - x0) (yc + y1 - y0)) r

envelope :: Diagram -> Envelope
envelope = hom envelopeP . unD

translate :: V2 -> Diagram -> Diagram
translate v = mkD . map (translateP v) . unD

myDiagram = mkD [
  Circle (P2 4 3) 2,
  Circle (P2 5 2) 5 ]

main =
  putStrLn $ show $ myDiagram
