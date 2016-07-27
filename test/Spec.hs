{-# LANGUAGE ViewPatterns #-}
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Category


instance (Arbitrary a, Arbitrary w) => Arbitrary (Writer w a) where
    arbitrary = do
        w <- arbitrary
        a <- arbitrary
        return $ Writer (a,w)


monoidAssocProp :: (Eq a, Monoids a) => a -> a -> a -> Bool
monoidAssocProp x y z = (x `append` y) `append` z == x `append` (y `append` z)

monoidRightIdProp :: (Eq a, Monoids a) => a -> Bool
monoidRightIdProp x = x == (x `append` empty)

monoidLeftIdProp :: (Eq a, Monoids a) => a -> Bool
monoidLeftIdProp x = x == (empty `append` x)


-- Functor Law
-- fmap id  =  id
-- fmap (g . f)  =  fmap g . fmap f

functorIdProp :: (Functors f, Eq (f a)) => f a -> Bool
functorIdProp x = (lift id x) == x

functorCompProp :: (Functors f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompProp x (apply -> f) (apply -> g) = (lift (g . f) x) == (lift g . lift f $ x)


-- Applicative Law
-- pure id <*> v = v                            -- Identity
-- pure f <*> pure x = pure (f x)               -- Homomorphism
-- u <*> pure y = pure ($ y) <*> u              -- Interchange
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
-- fmap f x = pure f <*> x                      -- fmap



-- Monad Law
-- bind m f = join (map f m)
-- join m = bind m id
-- map f m = bind m (unit . f)
-- apply fs xs = join $ map (\x -> map ($x) fs) xs
-- apply fs xs = bind xs (\x -> map ($x) fs) 
-- apply fs xs = do {f <- fs; x <- xs; unit (f x)}

monadRightIdProp :: (Monads m, Eq (m a)) => m a -> Bool
monadRightIdProp x = (bind x unit) == x

monadLeftIdProp :: (Monads m, Eq (m b)) => a -> Fun a (m b) -> Bool
monadLeftIdProp x (apply -> f) = bind (unit x) f == (f x)

monadAssocProp :: (Monads m, Eq (m c)) => m a -> Fun a (m b) -> Fun b (m c) -> Bool
monadAssocProp x (apply -> f) (apply -> g) = (bind (bind x f) g) == (bind x (\x' -> bind (f x') g))




monadBindProp :: (Monads m , Eq (m b)) => m a -> Fun a (m b) -> Bool
monadBindProp x (apply->f) = join (lift f x) == bind x f

monadMapProp :: (Monads m , Eq (m b)) => Fun a b -> m a -> Bool
monadMapProp (apply->f) x = lift f x == bind x (unit.f)

main :: IO ()
main = do
    -- List Monad
    quickCheck (monoidAssocProp :: [Int] -> [Int] -> [Int] -> Bool)
    quickCheck (monoidRightIdProp :: [Int] -> Bool)
    quickCheck (monoidLeftIdProp :: [Int] -> Bool)
    quickCheck (functorIdProp :: [Int] -> Bool)
    quickCheck (functorCompProp :: [Int] -> Fun Int Int -> Fun Int Int -> Bool)
    quickCheck (monadRightIdProp :: [Int] -> Bool)
    quickCheck (monadLeftIdProp :: Int -> Fun Int [Int] -> Bool)
    quickCheck (monadAssocProp ::  [Int] -> Fun Int [Char] -> Fun Char [Bool] -> Bool)

    -- Writer Monad
    quickCheck (functorIdProp :: Writer String Int -> Bool)
    quickCheck (monadBindProp :: Writer String Int -> Fun Int (Writer String Int) -> Bool)
    quickCheck (monadMapProp :: Fun Int Char -> Writer String Int -> Bool)