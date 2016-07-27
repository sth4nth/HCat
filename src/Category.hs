module Category where

class Semigroups m where
    append :: m -> m -> m

class Semigroups m => Monoids m where
    empty :: m

class Functors f where
	-- fmap, liftM
    lift :: (a -> b) -> f a -> f b

class Functors f => Applicatives f where
	-- pure, return
    unit :: a -> f a
    -- <*>, ap
    deploy :: f (a -> b) -> f a -> f b

class Applicatives m => Monads m where
    join :: m (m a) -> m a
    -- >>=
    bind :: m a -> (a -> m b) -> m b

-- List Monad
instance Semigroups [a] where
    append = (++)

instance Monoids [a] where
    empty = []

instance Functors [] where
    lift = map

instance Applicatives [] where
    unit x = [x]
    deploy fs xs = [f x| f <- fs, x <- xs]

instance Monads [] where
    join = concat
    bind xs k = concatMap k xs

-- ZipList Applicative
newtype ZipList a = ZipList [a] deriving (Show, Eq)

instance Semigroups (ZipList a) where
    append (ZipList a) (ZipList a') = ZipList (a ++ a')

instance Monoids (ZipList a) where
    empty = ZipList []

instance Functors ZipList where
    lift f (ZipList xs) = ZipList (map f xs)

instance Applicatives ZipList where
    unit a = ZipList (repeat a)
    deploy (ZipList fs) (ZipList xs) = ZipList (zipWith ($) fs xs)

-- Maybe Monad
data Option a = None | Some a deriving (Show, Eq)

instance Semigroups a => Semigroups (Option a) where
    append None a = a
    append a None = a
    append (Some a) (Some b) = Some (append a b)

instance Semigroups a => Monoids (Option a) where
    empty = None

instance Functors Option where
    lift _ None = None
    lift f (Some a) = Some (f a)

instance Applicatives Option where
    unit = Some
    deploy None _ = None
    deploy (Some f)  a = lift f a

instance Monads Option where
    join None = None
    join (Some a) = a
    bind None _ = None
    bind (Some a) k = k a

-- Writer Monad
newtype Writer w a = Writer (a, w) deriving (Show, Eq)

instance Functors (Writer w) where
    lift f (Writer (a, w)) = Writer (f a, w)

instance (Monoids w) => Applicatives (Writer w) where
    unit a = Writer (a, empty)
    deploy (Writer (f, w)) (Writer (x, w')) = Writer (f x, append w w')

instance (Monoids w) => Monads (Writer w) where
    join (Writer (Writer (a, w), w')) = Writer (a, append w' w)
    bind (Writer (a, w)) k =
        let (Writer (a', w')) = k a
        in Writer (a', append w w')

-- Reader Monad
newtype Reader e a = Reader { runReader :: e -> a }

instance Functors (Reader e) where
    lift f m = Reader $ f . runReader m

instance Applicatives (Reader e) where
    unit a     = Reader $ const a
    deploy mf ma = Reader $ \e ->
        let f  = runReader mf e
            a  = runReader ma e
        in f a

instance Monads (Reader e) where
    join m = Reader $ \e ->
        let m' = runReader m e
        in runReader m' e
    bind m k = Reader $ \e ->
        let a = runReader m e
        in runReader (k a) e

-- State Monad
newtype State s a = State { runState :: s -> (a, s) }

instance Functors (State s) where
    lift f m = State $ \s ->
        let (a, s') = runState m s
        in (f a, s')

instance Applicatives (State s) where
    unit a = State $ \s -> (a, s)
    deploy mf ma = State $ \s ->
        let (f, sf) = runState mf s
            (a, sfa) = runState ma sf
        in (f a, sfa)

instance Monads (State s) where
    join m = State $ \s ->
        let (m', s') = runState m s
        in runState m' s'
    bind m k = State $ \s ->
        let (a, s') = runState m s
        in runState (k a) s'

-- Continuation Monad
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functors (Cont r) where
    lift f m = Cont $ \x -> runCont m (x . f)

instance Applicatives (Cont r) where
    unit a = Cont ($a)
    deploy mf ma = Cont $ \x -> runCont ma (\a -> runCont mf (x . ($a)))

instance Monads (Cont r) where
    join m = Cont $ \x -> runCont m (\a -> runCont a x)
    bind m k = Cont $ \x -> runCont m (\a -> runCont (k a) x)
