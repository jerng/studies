import Control.Applicative
import Data.Function
import Data.Functor

main =  do  
            putStrLn "\n***Program Began.\n*\n*\n*\n\
                \This module is a study of the expression \"(map.const)\
                \ 1 [1,2,3]\""
            putStrLn $ "... which as coded above, computes to: " ++ show x ++ 
                "\n"
            putStrLn $ "Now we attempt to inline all the code from (map), (.), \
                \and (const) to see how it works:\n\n\

\First, we define:\n\
\\t\txDot f g = \\x -> f (g x)\n\
\... alternatively... \n\
\\t\txDot = \\f -> \\g -> \\x -> f (g x)\n\n\
\\tThen we write\n\
\\t\txDot map const 1 [1,2,3]\n\n\
\\t... which gives us:\n\
\\t\t" ++ show (xDot map const 1 [1,2,3]) ++ "\n\n\

\Second, we define:\n\
\\t\txMap _ []     = [] \n\
\\t\txMap f (x:xs) = f x : xMap f xs \n\
\... alternatively... \n\
\\t\txMap = \\f -> \\list ->   case list of\n\
\\t\t                              []      -> []\n\
\\t\t                              x:xs    -> f x : xMap f xs\n\n\
\\tThen we write\n\
\\t\txDot xMap const 1 [1,2,3]\n\n\
\\t... which gives us:\n\
\\t\t" ++ show (xDot xMap const 1 [1,2,3]) ++ "\n\n\

\Third, we define:\n\
\\t\txConst x _ = x\n\
\... alternatively... \n\
\\t\txConst = \\x -> \\_ -> x\n\
\\tThen we write\n\
\\t\txDot xMap xConst 1 [1,2,3]\n\n\
\\t... which gives us:\n\
\\t\t" ++ show (xDot xMap xConst 1 [1,2,3]) ++ "\n\n\

\Finally, we define:\n\
\\t\t\n\

\\t\txDotXConst =\n\
\\t\t    \\f -> \n\
\\t\t        \\x -> \n\
\\t\t            f ( g x )\n\
\\t\t            where \n\
\\t\t                g = \\y -> \n\
\\t\t                        \\_ -> y\n\
\\t\t            \n\
\\t\txMapXDot = \n\
\\t\t    \\g -> \n\
\\t\t        \\x -> \n\
\\t\t            f (g x)\n\
\\t\t            where\n\
\\t\t                f = \\fn -> \n\
\\t\t                        \\list ->   \n\
\\t\t                            case list of \n\
\\t\t                               []      -> []\n\
\\t\t                               z:zs    -> fn z : f fn zs\n\
\\t\t\n\
\\t\txMapXDotXConst = \n\
\\t\t    \\x -> \n\
\\t\t        f (g x)\n\
\\t\t        where\n\
\\t\t            g = \\y -> \n\
\\t\t                    \\_ -> y\n\
\\t\t            f = \\fn -> \n\
\\t\t                    \\list ->   \n\
\\t\t                        case list of \n\
\\t\t                           []      -> []\n\
\\t\t                           z:zs    -> fn z : f fn zs\n\
\\t\t            \n\
\\t\txMapXDotXConstOne = \n\
\\t\t    f (g 1)\n\
\\t\t    where\n\
\\t\t        g = \\y -> \n\
\\t\t                \\_ -> y\n\
\\t\t        f = \\fn -> \n\
\\t\t                \\list ->   \n\
\\t\t                    case list of \n\
\\t\t                       []      -> []\n\
\\t\t                       z:zs    -> fn z : f fn zs\n\

\\tThen we write\n\
\\t\t\n\n\
\\t\txDotXConst map  1 [1,2,3]\n\
\\t... which gives us:\n\
\\t\t" ++ show (xDotXConst map  1 [1,2,3]) ++ "\n\n\

\\tThen we write\n\
\\t\t\n\n\
\\t\txMapXDot const  1 [1,2,3]\n\
\\t... which gives us:\n\
\\t\t" ++ show (xMapXDot const  1 [1,2,3]) ++ "\n\n\

\\tThen we write\n\
\\t\t\n\n\
\\t\txMapXDotXConst  1 [1,2,3]\n\
\\t... which gives us:\n\
\\t\t" ++ show (xMapXDotXConst  1 [1,2,3]) ++ "\n\n\

\\tThen we write\n\
\\t\t\n\n\
\\t\txMapXDotXConstOne [1,2,3]\n\
\\t... which gives us:\n\
\\t\t" ++ show (xMapXDotXConstOne [1,2,3]) ++ "\n\n\


\"

            putStrLn "*\n*\n*** Program Ended."

x = ( (map.const) 1 [1,2,3] )

--xDot f g = \x -> f (g x)
xDot = 
    \f -> 
        \g -> 
            \x -> f (g x)

--xMap _ []     = []
--xMap f (x:xs) = f x : xMap f xs
xMap = 
    \fn -> 
        \list ->   
            case list of 
               []      -> []
               x:xs    -> fn x : xMap fn xs

--xConst x _ = x
xConst = 
    \x -> 
        \_ -> -- STUDY THIS: what happens when (.) is applied to (const) ?
            x

xDotXConst =
    \f -> 
        \x -> 
            f ( g x )
            where 
                g = \y -> 
                        \_ -> y
            
xMapXDot = 
    \g -> 
        \x -> 
            f (g x)
            where
                f = \fn -> 
                        \list ->   
                            case list of 
                               []      -> []
                               z:zs    -> fn z : f fn zs

xMapXDotXConst = 
    \x -> 
        f (g x)
        where
            g = \y -> 
                    \_ -> y
            f = \fn -> 
                    \list ->   
                        case list of 
                           []      -> []
                           z:zs    -> fn z : f fn zs
            
xMapXDotXConstOne = 
    f (g 1)
    where
        g = \y -> 
                \_ -> y
        f = \fn -> 
                \list ->   
                    case list of 
                       []      -> []
                       z:zs    -> fn z : f fn zs


