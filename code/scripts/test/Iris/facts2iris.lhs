

> module Main where
>
> import Char
> import System.Environment

> main =
>     let
>     checkHeader = \x -> length $ words x
>     processContents = \x ->
>             do
>               line <- getLine
>               if null line
>                 then return ()
>                 else do
>                    putStr $ transFact line x
>                    processContents x
>     in do            
>        a:args <- getArgs
>        line <- getLine
>        if null line
>          then return ()
>          else processContents a

            oContents <- unpack $ transFact  pack iContents
       writeFile oFile iContents

> transFact :: String -> String -> String
> 
> transFact fact a = a ++ "(" ++ genConstraint args ++ ")."
>     where
>     args = words fact
>     n = length args

> genConstraint :: [String] -> String
> 
> genConstraint [c]    = c
> genConstraint (c:cs) = c ++ "," ++ genConstraint cs
