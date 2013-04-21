main = return 1


-- | Invented for use where it is less noisy to use (a.b.c...d)
-- instead of (a.b.c $ d) 
--
-- Also achieves things like (map ... T.pack.show $ fst (1,2) )
--
-- Perhaps to be avoided where it may be too subtle to use (a b... c.d.e... f g)
-- instead of (a b $ c.d.e $ f g)
(...) :: (a -> b) -> a -> b
f ... a = f a
infixr 8 ...

-- The mirror of (...)
(>...) :: a -> (a -> b) -> b
a >... f = f a
infixl 8 >...


-- The mirror of (.)
(>.) :: (a -> b) -> (b -> c) -> a -> c 	-- Defined in `GHC.Base'
g >. f = \x -> f (g x)
infixl 9 >.

