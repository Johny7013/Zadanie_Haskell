-- Student: Jan Klinkosz, student id number: 394 342
module PPrint where
import Data.List (intersperse)

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showString " "

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep list = foldr (.) id $ Data.List.intersperse sep list

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f list = pprV (map f list)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
