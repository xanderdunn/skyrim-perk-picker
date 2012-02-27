--Skyrim Proper Perk App 0.1
--Optimize the usefulness of a player's perks by asking a player for
--  his preferences and optimize the total utility given a constraining
--  number of perks

--There are ? perks, but a reasonable player will achieve only 49.  A dedicated player might achieve 69.
--This app will help with perk selection by optimizing the total utility of your perk preferences.
--This app does not pretend either to know game mechanics or how you want to play the game.  
--This app will ask for your prefences and then output a tree of perks you should get.
--Value perks from 0 through 5, where 0 is a waste of a perk point and 5 is a perk that you really want.
--Give a value of 7 to any perk you must have or that you have already selected.
--Default value is 0

module Main where
import AppModel
import Data.List
--import Data.Char --toUpper, isSpace, digitToInt
--import Data.List

--Must take a perk set so that it can forward it along to the
--  next function
--asker :: [Perk] -> IO()
--asker ps = do
--    str <- getLine
--    dialogueHandler ps str

--Present a list of perks for formating
--presenter :: [Perk] -> String
--presenter [] = ""
--presenter ps = show(firstperk) ++ presenter (tail ps)
--    where
--        firstperk = head ps

--trim :: String -> String
--trim = f . f
--    where f = reverse . dropWhile isSpace

--Figure out what to do with strings entered into dialogue boxes
--dialogueHandler :: [Perk] -> String -> IO()
--dialogueHandler ps str
--    | map toUpper str `elem` ["SAVE"] = do
--        savePerkSet ps
--        putStrLn "All perks and their preferences have been saved to an editable text file titled perksData.hs, probably in your user directory"
--        asker ps
--    | map toUpper str `elem` ["LOAD"] = do
--        perkstr <- loadPerkSet
--        let newperkstr = splitList (removeStuff perkstr)
--        let newps = parseStringToPerkSet (init newperkstr)
--        putStrLn "File loaded."
--        asker newps
--        --Print the perks of a given skill
--    | take 4 (map toUpper str) `elem` ["SHOW"]= do
--        putStrLn $ presenter $ getPerks ps (drop 5 str)
--        asker ps
--    | '=' `elem` str = do
--        --Update the preference value of the perk in question
--        let i = digitToInt (last str)
--        if i `elem` [0,1,2,3,4,5,7]
--            then do
--                let newps = updatePref ps (init $ takeWhile (/='=') (trim str)) i
--                asker newps
--            else do
--                putStrLn "Your input was not valid."
--                asker ps
--    | take 6 (map toUpper str) == "FINISH" && length str == 9 = do
--        let i = read (drop 7 str) :: Int
--        if i >= 1 && i <= 69
--            then do
--                putStrLn "================================================"
--                putStrLn "This is your final perk list:"
--                --A list of perks 
--                let magicPerks = perkListPreqs ps (nub $ perksOfPref (updatetvd ps []) 7) []
--                --Find the first 49 perks
--                let fs = finish ps magicPerks i
--                putStrLn $ presenter $ fs
--            else do
--                putStrLn "The number you entered is not valid."
--                asker ps
--    | otherwise = do
--        putStrLn "Your input was not valid."
--        asker ps

--preferencesKey :: IO()
--preferencesKey = do
--    putStrLn ""
--    putStrLn "~ "
--    putStrLn "~ "
--    putStrLn "~ "
--    putStrLn "The warrior skills you can choose from are: archery, block, heavy armor, one handed, two handed, smithing"
--    putStrLn "The mage skills you can choose from are: destruction, alteration, restoration, illusion, conjuration, alternation, enchanting"
--    putStrLn "The thief skills you can choose from are: alchmey, light armor, lock picking, pickpocket, sneak, speech"
--    putStrLn ""

list :: IO()
list = do
    savePerkSet initial "perkData.txt"
    putStrLn "The initial perk list has been saved to your user directory."
    putStrLn "Now open that text file and edit the right-most zeroes into numbers of your preference from 1-5 or 7 for a must-have."

--TODO: Check if the perkData.txt file exists.  If not, then save out the template
--TODO: Separate the list of 49 and the list of 20
main :: IO()
main = do
    location <- getFileLoc "perkData.txt"
    putStrLn $ "This app will load the perk preferences from the file " ++ location
    putStrLn $ "Then, a list of 49 + 20 perks will be saved to perkList.txt in the same folder."
    --Load the perk data file, which should be in the user directory
    perkstr <- loadPerkSet
    let newperkstr = splitList (removeStuff perkstr)
    let newps = parseStringToPerkSet (init newperkstr)
    --Now create and print the final list of perks
    --Somehow, perklistPreqs can introduce duplicates on rare occasion, so they are removed here
    let magicPerks = nub $ perkListPreqs newps (perksOfPref (updatetvd newps []) 7) []
    let fs1 = finish newps magicPerks 49
    let fs2 = finish [x | x <- newps, x `notElem` fs1] [] 20
    savePerkSet (fs1 ++ fs2) "perkList.txt"
    putStrLn "Done."