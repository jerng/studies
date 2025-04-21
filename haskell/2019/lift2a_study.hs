import Control.Applicative
import Data.Function
import Data.Functor

main =  do  putStrLn $ "\n*** Program Began.\n*\n*\n*\n\n\

\Consider the expression \"(+) <$> (Just 1) <*> (Just 2)\", pronounced:\n\n\
\\t\"the binary-addition-operator is mapped into the functorial Int (Just 1),\n\
\\tresulting in the functorial section (Just (1+)); that section is then applied\n\
\\tover the functorial Int (Just 2), resulting in the final functorial Int \
\(" ++ show ((+) <$> (Just 1) <*> (Just 2)) ++ ").\n\n\


\An equivalent expression \"liftA2 (+) (Just 1) (Just 2)\", simply applies\n\
\the ternary function (liftA2) to three arguments. Of interest is the definition\n\
\of (liftA2) which is in terms of (<*>) \"applied over\". \n\n\

\The default, knotted, definitions are:\n\n\
\\t(<*>) :: f (a -> b) -> f a -> f b\n\
\\t(<*>) = liftA2 id\n\n\
\\tliftA2 :: (a -> b -> c) -> f a -> f b -> f c\n\
\\tliftA2 f x = (<*>) (fmap f x)\n\n\

\However, implementing the above for the type Maybe causes a space leak\n\
\(circular definition?). Refer to GHC.Base's instance declaration, for\n\
\Applicative Maybe to see the actual implementation. \
\\t\
\\t\
\\t\
\\t\
\\t\
\\t\
\\t\

\\n\n*\n*\n*\n*** Program Ends.\n\n"

