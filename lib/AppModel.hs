module AppModel (Perk, finish, initial, updatePref, getPerks, updatetvd, perkIdentifier, perkListPreqs, 
    perksOfPref, savePerkSet, loadPerkSet, removeStuff, splitList, parseStringToPerkSet, getFileLoc) where

import Data.List --filter, tails
import Data.Char --toLower
import System.Directory --getUserAppDirectory
import System.FilePath.Posix
import Data.List.Split --split
--import Debug.Trace

--A perk is a 5-tuple (Total Value Density, School, Skill, Name, Description, Prerequisite Perk, Preference)
data Perk = NULL | PerkImpl (Double, String, String, String, String, String, Int)

--TODO: See about not needing null
instance Show Perk where
    show NULL = "NULL.  This is an error. You have somehow tried to display a perk which does not exist."
    --show p = id ++ " = " ++ show(tvd)
    --show p = show(tvd) ++ " " ++ skill ++ "   " ++ name ++ ": " ++ def ++ " | Preference = " ++ (show pref) ++ "\n" ++ ";"
    show p = skill ++ "   " ++ name ++ ": " ++ def ++ " | TVD = " ++ show(tvd) ++ " | Preference = " ++ (show pref) ++ ";" ++ "\n"
        where
            PerkImpl (tvd, _, skill, name, def, _, pref) = p

--Perks are equal if their id's are equal
--EQ must be overloaded to compare only the name because
--  we are overloading Ord with the tvd number, and tvd numbers
--  change over time within a given function
instance Eq Perk where
    p1 == p2 = name1 == name2
        where
            PerkImpl (_, _, _, name1, _, _, _) = p1
            PerkImpl (_, _, _, name2, _, _, _) = p2

--Compare perks by their tvd
instance Ord Perk where
    --Perk ids must be unique
    p1 `compare` p2 = tvd1 `compare` tvd2
        where
            PerkImpl (tvd1, _, _, _, _, _, _) = p1
            PerkImpl (tvd2, _, _, _, _, _, _) = p2

--calctvn :: [Perk] -> [Perk] -> Perk -> Int
--calctvn ps fs p = sum $ map getPref ppreqs
--    where
--        ppreqs = [i | i <- perkAndPreqs ps p, i `notElem` fs]

calctvn :: [Perk] -> [Perk] -> Perk -> Perk
calctvn ps fs p = PerkImpl (fromIntegral(sum $ map getPref ppreqs), school, skill, name, def, pregstring, pref)
    where
        ppreqs = [i | i <- perkAndPreqs ps p, i `notElem` fs]
        PerkImpl (_, school, skill, name, def, pregstring, pref) = p

calctvd :: [Perk] -> [Perk] -> Perk -> Perk
calctvd ps fs p = PerkImpl (tvn / fromIntegral(points), school, skill, name, def, pregstring, pref)
    where
        tvnp = calctvn ps fs p
        points = length $ [i | i <- perkAndPreqs ps tvnp, i `notElem` fs]
        PerkImpl (tvn, school, skill, name, def, pregstring, pref) = tvnp

--calctvd :: [Perk] -> [Perk] -> Perk -> Double
--calctvd ps fs p = fromIntegral(tvn) / fromIntegral(points)
--    where
--        tvn = calctvn ps fs p
--        points = length $ [i | i <- perkAndPreqs ps p, i `notElem` fs]

--Take a perk and return its preference value
getPref :: Perk -> Int
getPref NULL = 0
getPref p = pref
    where
        PerkImpl (_, _, _, _, _, _, pref) = p

--Take a perk set ps and a perk p and return a list of all 
perkAndPreqs :: [Perk] -> Perk -> [Perk]
perkAndPreqs _ NULL = []
perkAndPreqs ps p = [p] ++ perkAndPreqs ps preq
    where
        PerkImpl (_, _, _, _, _, preqstring, _) = p
        preq = perkIdentifier ps preqstring

--Take a list of perks and return a list of those perks and their prerequisites
--ps = perk set I am pulling preqs from
--perks = list of perks whose preqs I want
--fs = perks in the final output list
perkListPreqs :: [Perk] -> [Perk] -> [Perk] ->[Perk]
--Somehow I can introduce duplicates in rare circumstances,
--  nub fixes this
perkListPreqs _ [] fs = fs
perkListPreqs ps perks fs = fs ++ perkListPreqs ps (tail excludedperks) newfs
    where
        excludedperks = [i | i <- perks, i `notElem` fs]
        newfs = [i | i <- perkAndPreqs ps p, i `notElem` fs]
        p = head excludedperks

--Take a perk set ps and a preference value i
--  and return a perk set of all perks with preference
--  i
perksOfPref :: [Perk] -> Int -> [Perk]
perksOfPref [] _ = []
perksOfPref ps i
    | pref == i = [p] ++ perksOfPref (tail ps) i
    | otherwise = perksOfPref (tail ps) i
    where
        p = head ps
        PerkImpl (_, _, _, _, _, _, pref) = p


--TOOD: For efficiency, ignore all perks with a preference of 0
--Take a list of candidate perks ps and construct a list of the final perks
--  make the initial call with fs = []
finish :: [Perk] -> [Perk] -> Int -> [Perk]
--finish ps fs _ | trace ("\nps = " ++ show ps ++ "\n" ++ "fs = " ++ show fs ++ "!!") False = []
finish ps fs n
    | length fs < n && length (fs ++ newfs) <= n = finish newps (fs ++ newfs) n
    --If adding a new perk and its prerequisites will bring me over the limit, then ignore it and move to the next perk
    | length fs < n && length (fs ++ newfs) > n = finish (tail (reverse (sort cleanedps))) fs n
    | otherwise = fs
    where
        --Remove any pre-existing fs items from 
        cleanedps = nub [i | i <- ps, i `notElem` fs]
        --ps with update tvd values
        ups = updatetvd cleanedps fs
        --This is the interesting value of the given list ps
        --inter = last $ sort ups
        inter = maximum ups
        --inter = last $ sort ups
        --A list of all values which might need to be added to fs 
        maybeNewfs = perkAndPreqs ps inter
        --Whittle that list down to a list of distinctly new elements
        newfs = filter (`notElem` fs) maybeNewfs
        --Now remove all items added to fs from the candidate list
        newps = filter (`notElem` maybeNewfs) ups

--Update the Total Value Density value for each perk in a perk list
updatetvd :: [Perk] -> [Perk] -> [Perk]
updatetvd ps fs = map (calctvd ps fs) ps

--Take a list of perks ps and return a list of the names of
--  all the perks in that list
namesList :: [Perk] -> [String]
namesList [] = []
namesList ps = [name] ++ namesList (tail ps)
    where
        p = head ps
        PerkImpl (_, _, _, name, _, _, _) = p

--Take a string representing the id of a perk in the perk set ps and return that perk
perkIdentifier :: [Perk] -> String -> Perk
perkIdentifier _ "" = NULL
perkIdentifier ps str
    --If the string matches the name, then done
    | map toLower str == map toLower name = p
    --If the string does not match the name but is in the list of possibilities, then continue
    | str `elem` namesList ps = perkIdentifier (tail ps) str
    --Otherwise, it does not exist
    | otherwise = NULL
    where
        p = head ps
        --Unpack the first perk in ps
        PerkImpl (_, _, _, name, _, _, _) = p

--Takes a perk and its preference value and retuns the perk with its updated preference value
updatePref :: [Perk] -> String -> Int -> [Perk]
--Return the same perk, but with a switch preference number
updatePref [] _ _ = []
updatePref ps str i
    | name /= str = [p] ++ updatePref (tail ps) str i
    | otherwise = [PerkImpl (tvn, school, skill, name, def, preqs, i)] ++ updatePref (tail ps) str i
    where
        p = head ps
        --Unpack the perk
        PerkImpl (tvn, school, skill, name, def, preqs, _) = p

--Function to create a list of perks in a certain school or skill
--Char s = a school set of perks
--Char k = a skill set of perks
getPerks :: [Perk] -> String -> [Perk]
getPerks [] _ = []
getPerks ps str = [i | i <- [perk], skill == (map toLower str)] ++ getPerks (tail ps) str
    where
        --The first perk in the list
        perk = head ps
        --Unpack the perk
        PerkImpl (_, _, skill, _, _, _, _) = perk

--Source: http://www.haskell.org/haskellwiki/99_questions/Solutions/26
--Take a list xs and return a list of all combinations of xs of length n
--combinations :: Int -> [a] -> [[a]]
--combinations 0 _  = [ [] ]
--combinations n xs = [ y:ys | y:xs' <- tails xs
--                           , ys <- combinations (n-1) xs']

--combinationsdo :: Int -> [a] -> [[a]]
--combinationsdo 0 _  = return []
--combinationsdo n xs = do 
--    y:xs' <- tails xs
--    ys <- combinationsdo (n-1) xs'
--    return (y:ys)

getFileLoc :: String -> IO String
getFileLoc str = do
    appdir <- getCurrentDirectory
    createDirectoryIfMissing False appdir
    return (combine appdir str)

--To create a template file, simply run 'savePerkSet initial'
savePerkSet :: [Perk] -> String -> IO ()
savePerkSet ps str = do
    dbFile <- getFileLoc str
    let tempFile = replaceExtension dbFile ".tmp"
    writeFile tempFile (show ps)
    renameFile tempFile dbFile

--Turn all the elements of a list of litss into a single element in a list
--concatElems :: [[a]] -> [a]
--concatElems [] = []
--concatElems ls = head ls ++ concatElems (tail ls)

removeStuff :: String -> String
removeStuff str = [i | i <- str, i /= '[', i /= ']', i/= '\n']

splitList :: [Char] -> [[Char]]
splitList str = splitOn ";" str

deleteUntilUpper :: String -> String
deleteUntilUpper [] = []
deleteUntilUpper l
    | isUpper p = l
    | otherwise = deleteUntilUpper (tail l)
    where
        p = head l

--TODO: This function maps the two different perks of the same perk to the same perk.
--Fix by creating a perkset that is initial and sipmly changing the value of all perks in that set which 
--are equivalent to the perk from the p in ps currently up for grabs
--Before passing load[Perk]'s output to this guy, I will need to 
--  unpack the IO String and remove the brackets at the start and end
parseStringToPerkSet :: [String] -> [Perk]
parseStringToPerkSet [] = []
parseStringToPerkSet l = [pnew] ++ parseStringToPerkSet (tail l)
    where
        --Get the first string to turn into a perk
        pstring = head l
        --Get rid of the unecessary characters at the beginning
        y = deleteUntilUpper pstring
        --Get the string representing the name of the perk
        findname = takeWhile (/=':') y
        --Get the int representing the perk
        i = read [last y] :: Int
        --Turn the name into a perk
        p = perkIdentifier initial findname
        PerkImpl (tvn, school, skill, name, def, preqs, _) = p
        pnew = PerkImpl (tvn, school, skill, name, def, preqs, i)

--  trying to load a file
loadPerkSet :: IO String
loadPerkSet = do
    dbFile <- getFileLoc "perkData.txt"
    input <- readFile dbFile
    return input

--The perk data
initial :: [Perk]
initial = [PerkImpl (1.0, "warrior", "archery", "Overdraw 1", "Bows do a total of 20% more than base damage.", "", 0),
        PerkImpl (0, "warrior", "archery", "Overdraw 2", "Bows do a total of 40% more than base damage.", "Overdraw 1", 0),
        PerkImpl (0, "warrior", "archery", "Overdraw 3", "Bows do a total of 60% more than base damage.", "Overdraw 2", 0),
        PerkImpl (0, "warrior", "archery", "Overdraw 4", "Bows do a total of 80% more than base damage.", "Overdraw 3", 0),
        PerkImpl (0, "warrior", "archery", "Overdraw 5", "Bows do a total of 100% more than base damage.", "Overdraw 4", 0),
        PerkImpl (0, "warrior", "archery", "Eagle Eye", "Pressing Block while aiming will zoom in your view.", "Overdraw 1", 0),
        PerkImpl (0, "warrior", "archery", "Critical Shot 1", "A total of a 10% chance of critical hit.", "Overdraw 1", 0),
        PerkImpl (0, "warrior", "archery", "Critical Shot 2", "A total of a 15% chance of critical hit.", "Critical Shot 1", 0),
        PerkImpl (0, "warrior", "archery", "Critical Shot 3", "A total of a 20% chance of critical hit.", "Critical Shot 2", 0),
        PerkImpl (0, "warrior", "archery", "Steady Hand 1", "Zooming in with a bow slows time by a total of 25%", "Eagle Eye", 0),
        PerkImpl (0, "warrior", "archery", "Steady Hand 2", "Zooming in with a bow slows time by a total of 50%", "Steady Hand 1", 0),
        PerkImpl (0, "warrior", "archery", "Power Shot", "50% change for arrows to stagger all but the largest opponents", "Eagle Eye", 0),
        PerkImpl (0, "warrior", "archery", "Hunter's Discipline", "Recover twice as many arrows from dead bodies", "Critical Shot 1", 0),
        PerkImpl (0, "warrior", "archery", "Ranger", "Able to move faster with a drawn bow", "Hunter's Discipline", 0),
        PerkImpl (0, "warrior", "archery", "Quick Shot", "Can draw a bow 30% faster", "Power Shot", 0),
        PerkImpl (0, "warrior", "archery", "Bullseye", "15% chance of paralyzing target for several seconds [Quick Shot]", "Quick Shot", 0),
        PerkImpl (0, "warrior", "archery", "Bullseye", "15% chance of paralyzing target for several seconds [Ranger]", "Ranger", 0),
        PerkImpl (0, "warrior", "block", "Shield Wall 1", "Blocking is a total of 20% more effective", "", 0),
        PerkImpl (0, "warrior", "block", "Shield Wall 2", "Blocking is a total of 25% more effective", "Shield Wall 1", 0),
        PerkImpl (0, "warrior", "block", "Shield Wall 3", "Blocking is a total of 30% more effective", "Shield Wall 2", 0),
        PerkImpl (0, "warrior", "block", "Shield Wall 4", "Blocking is a total of 35% more effective", "Shield Wall 3", 0),
        PerkImpl (0, "warrior", "block", "Shield Wall 5", "Blocking is a total of 40% more effective", "Shield Wall 4", 0),
        PerkImpl (0, "warrior", "block", "Deflect Arrows", "Arrows that hit the shield do no damage", "Shield Wall 1", 0),
        PerkImpl (0, "warrior", "block", "Quick Reflexes", "Time slows down if you are blocking during an enemy's power attack", "Shield Wall 1", 0),
        PerkImpl (0, "warrior", "block", "Power Bash", "Able to do a power bash", "Shield Wall 1", 0),
        PerkImpl (0, "warrior", "block", "Elemental Protection", "Blocking with a shield reduces incoming fire, frost and shock damage by 50%", "Deflect Arrows", 0),
        PerkImpl (0, "warrior", "block", "Deadly Bash", "Bashing does five times more damage", "Power Bash", 0),
        PerkImpl (0, "warrior", "block", "Block Runner", "Able to move faster with a shield raised", "Elemental Protection", 0),
        PerkImpl (0, "warrior", "block", "Disarming Bash", "Chance to disarm when power bashing", "Deadly Bash", 0),
        PerkImpl (0, "warrior", "block", "Shield Charge", "Sprinting with a shield raised knocks down most targets [Block Runner]", "Block Runner", 0),
        PerkImpl (0, "warrior", "block", "Shield Charge", "Sprinting with a shield raised knocks down most targets [Disarming Bash]", "Disarming Bash", 0),
        PerkImpl (0, "warrior", "heavy armor", "Juggernaut 1", "Heavy Armor is a total of 20% higher than base armor rating.", "", 0),
        PerkImpl (0, "warrior", "heavy armor", "Juggernaut 2", "Heavy Armor is a total of 40% higher than base armor rating.", "Juggernaut 1", 0),
        PerkImpl (0, "warrior", "heavy armor", "Juggernaut 3", "Heavy Armor is a total of 60% higher than base armor rating.", "Juggernaut 2", 0),
        PerkImpl (0, "warrior", "heavy armor", "Juggernaut 4", "Heavy Armor is a total of 80% higher than base armor rating.", "Juggernaut 3", 0),
        PerkImpl (0, "warrior", "heavy armor", "Juggernaut 5", "Heavy Armor is a total of 100% higher than base rating.", "Juggernaut 4", 0),
        PerkImpl (0, "warrior", "heavy armor", "Fists of Steel", "Unarmed attacks with Heavy Armor gauntlets do their armor rating in extra damage", "Juggernaut 1", 0),
        PerkImpl (0, "warrior", "heavy armor", "Well Fitted", "25% armor bonus if wearing all Heavy Armor: head, chest, hands, feet", "Juggernaut 1", 0),
        PerkImpl (0, "warrior", "heavy armor", "Cushioned", "Half damage from falling if wearing all Heavy Armor: head, chest, hands, feet", "Fists of Steel", 0),
        PerkImpl (0, "warrior", "heavy armor", "Tower of Strength", "50% less stagger when wearing only Heavy Armor", "Well Fitted", 0),
        PerkImpl (0, "warrior", "heavy armor", "Conditioning", "Heavy Armor weighs nothing and doesn't slow you down when worn", "Cushioned", 0),
        PerkImpl (0, "warrior", "heavy armor", "Matching Set", "Additional 25% armor bonus if wearing a matched set of Heavy Armor", "Tower of Strength", 0),
        PerkImpl (0, "warrior", "heavy armor", "Reflect Blows", "10% chance to reflect melee damage back to the enemy while wearing all Heavy Armor: head, chest, hands, feet", "Matching Set", 0),
        PerkImpl (0, "warrior", "one handed", "Armsman 1", "One-Handed weapons do a total of 20% more than base damage.  ", "", 0),
        PerkImpl (0, "warrior", "one handed", "Armsman 2", "One-Handed weapons do a total of 40% more than base damage.", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Armsman 3", "One-Handed weapons do a total of 60% more than base damage.", "Armsman 2", 0),
        PerkImpl (0, "warrior", "one handed", "Armsman 4", "One-Handed weapons do a total of 80% more than base damage", "Armsman 3", 0),
        PerkImpl (0, "warrior", "one handed", "Armsman 5", "One-Handed weapons do a total of 100% more than base damage", "Armsman 4", 0),
        PerkImpl (0, "warrior", "one handed", "Fighting Stance", "Power attacks with one-handed weapons cost 20% less stamina", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Hack and Slash 1", "Attacks with war axes cause extra bleeding damage.", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Hack and Slash 2", "Attacks with war axes cause extra bleeding damage.", "Hack and Slash 1", 0),
        PerkImpl (0, "warrior", "one handed", "Hack and Slash 3", "Attacks with war axes cause extra bleeding damage.", "Hack and Slash 2", 0),
        PerkImpl (0, "warrior", "one handed", "Bone Breaker 1", "Attacks with maces ignore a total of 25% of armor.", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Bone Breaker 2", "Attacks with maces ignore a total of 50% of armor.", "Bone Breaker 1", 0),
        PerkImpl (0, "warrior", "one handed", "Bone Breaker 3", "Attacks with maces ignore a total of 75% of armor.", "Bone Breaker 2", 0),
        PerkImpl (0, "warrior", "one handed", "Bladesman 1", "Attacks with swords have a total 10% chance of doing critical damage.", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Bladesman 2", "Attacks with swords have a total 15% chance of doing critical damage.", "Bladesman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Bladesman 3", "Attacks with swords have a total 20% chance of doing critical damage.", "Bladesman 2", 0),
        PerkImpl (0, "warrior", "one handed", "Dual Flurry 1", "Dual wielding attacks are a total 20% faster", "Armsman 1", 0),
        PerkImpl (0, "warrior", "one handed", "Dual Flurry 2", "Dual wielding attacks are a total 35% faster", "Dual Flurry 1", 0),
        PerkImpl (0, "warrior", "one handed", "Savage Strike", "Standing power attacks do 25% bonus damage with a chance to decapitate your enemies", "Fighting Stance", 0),
        PerkImpl (0, "warrior", "one handed", "Critical Charge", "Can do a one-handed power attack while sprinting that does double critical damage", "Fighting Stance", 0),
        PerkImpl (0, "warrior", "one handed", "Dual Savagery", "Dual wielding power attacks do 50% bonus damage", "Dual Flurry", 0),
        PerkImpl (0, "warrior", "one handed", "Paralyzing Strike", "Backwards power attack has a 25% chance to paralyze the target [Savage Strike]", "Savage Strike", 0),
        PerkImpl (0, "warrior", "one handed", "Paralyzing Strike", "Backwards power attack has a 25% chance to paralyze the target [Savage Strike]", "Critical Charge", 0),
        PerkImpl (0, "warrior", "two handed", "Barbarian 1", "Two-Handed weapons do a total of 20% more damage", "", 0),
        PerkImpl (0, "warrior", "two handed", "Barbarian 2", "Two-Handed weapons do a total of 40% more damage", "Barbarian 1", 0),
        PerkImpl (0, "warrior", "two handed", "Barbarian 3", "Two-Handed weapons do a total of 60% more damage", "Barbarian 2", 0),
        PerkImpl (0, "warrior", "two handed", "Barbarian 4", "Two-Handed weapons do a total of 80% more damage", "Barbarian 3", 0),
        PerkImpl (0, "warrior", "two handed", "Barbarian 5", "Two-Handed weapons do a total of 100% more damage", "Barbarian 4", 0),
        PerkImpl (0, "warrior", "two handed", "Champion's Stance", "Power attacks with two-handed weapons cost 25% less stamina", "Barbarian 1", 0),
        PerkImpl (0, "warrior", "two handed", "Limbsplitter 1", "Attacks with battle axes cause extra bleeding damage (Additional ranks raise the bleeding damage)", "Barbarian 1", 0),
        PerkImpl (0, "warrior", "two handed", "Limbsplitter 2", "Attacks with battle axes cause extra bleeding damage (Additional ranks raise the bleeding damage)", "Limbsplitter 1", 0),
        PerkImpl (0, "warrior", "two handed", "Limbsplitter 3", "Attacks with battle axes cause extra bleeding damage (Additional ranks raise the bleeding damage)", "Limbsplitter 2", 0),
        PerkImpl (0, "warrior", "two handed", "Deep Wounds 1", "Attacks with greatswords have a 10% total chance of doing critical damage", "Barbarian 1", 0),
        PerkImpl (0, "warrior", "two handed", "Deep Wounds 2", "Attacks with greatswords have a 15% total chance of doing critical damage", "Deep Wounds 1", 0),
        PerkImpl (0, "warrior", "two handed", "Deep Wounds 3", "Attacks with greatswords have a 20% total chance of doing critical damage", "Deep Wounds 2", 0),
        PerkImpl (0, "warrior", "two handed", "Skullcrusher 1", "Attacks with warhammers ignore a total of 25% of armor", "Barbarian 1", 0),
        PerkImpl (0, "warrior", "two handed", "Skullcrusher 2", "Attacks with warhammers ignore a total of 50% of armor", "Skullcrusher 1", 0),
        PerkImpl (0, "warrior", "two handed", "Skullcrusher 3", "Attacks with warhammers ignore a total of 75% of armor", "Skullcrusher 2", 0),
        PerkImpl (0, "warrior", "two handed", "Devastating Blow", "Standing power attacks do 25% bonus damage with a chance to decapitate your enemies", "Champion's Stance", 0),
        PerkImpl (0, "warrior", "two handed", "Great Critical Charge ", "Can do a two-handed power attack while sprinting that does double critical damage", "Champion's Stance", 0),
        PerkImpl (0, "warrior", "two handed", "Sweep", "Sideways power attacks with two-handed weapons hit all targets in front of you [Devastating Blow]", "Devastating Blow", 0),
        PerkImpl (0, "warrior", "two handed", "Sweep", "Sideways power attacks with two-handed weapons hit all targets in front of you [Great Critical Charge]", "Great Critical Charge", 0),
        PerkImpl (0, "warrior", "two handed", "Warmaster", "Backwards power attack has a 25% chance to paralyze the target", "Sweep", 0),
        PerkImpl (0, "warrior", "smithing", "Steel Smithing", "Can create Steel armor and weapons at forges, and improve them twice as much", "", 0),
        PerkImpl (0, "warrior", "smithing", "Elven Smithing", "Can create Elven armor and weapons at forges, and improve them twice as much", "Steel Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Dwarven Smithing", "Can create Dwarven armor and weapons at forges, and improve them twice as much", "Steel Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Advanced Armors", "Can create Scaled and Plate armor at forges, and improve them twice as much", "Elven Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Orcish Smithing", "Can create Orcish armor and weapons at forges, and improve them twice as much", "Dwarven Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Arcane Blacksmith", "You can improve magical weapons and armor", "Steel Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Glass Smithing", "Can create Glass armor and weapons at forges, and improve them twice as much", "Elven Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Ebony Smithing", "Can create Ebony armor and weapons at forges, and improve them twice as much", "Orcish Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Daedric Smithing", "Can create Daedric armor and weapons at forges, and improve them twice as much", "Ebony Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Dragon Armor", "Can create Dragon armor at forges, and improve them twice as much [Glass Smithing]", "Glass Smithing", 0),
        PerkImpl (0, "warrior", "smithing", "Dragon Armor", "Can create Dragon armor at forges, and improve them twice as much [Deadric Smithing]", "Daedric Smithing", 0),
        PerkImpl (0, "mage", "alteration", "Novice Alteration", "Cast Novice level Alteration spells for half magicka", "", 0),
        PerkImpl (0, "mage", "alteration", "Alteration Dual Casting", "Dual casting an Alteration spell overcharges the effects into an even more powerful version", "Novice Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Apprentice Alteration", "Cast Apprentice level Alteration spells for half magicka", "Novice Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Mage Armor 1", "Protection spells like Stoneflesh are 2x stronger than base if not wearing armor", "Apprentice Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Mage Armor 2", "Protection spells like Stoneflesh are 2.5x stronger than base if not wearing armor", "Mage Armor 1", 0),
        PerkImpl (0, "mage", "alteration", "Mage Armor 3", "Protection spells like Stoneflesh are 3x stronger than base if not wearing armor", "Mage Armor 2", 0),
        PerkImpl (0, "mage", "alteration", "Magic Resistance 1", "Block a total of 10% of all spell effects", "Apprentice Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Magic Resistance 2", "Block a total of 20% of all spell effects", "Magic Resistance 1", 0),
        PerkImpl (0, "mage", "alteration", "Magic Resistance 3", "Block a total of 30% of all spell effects", "Magic Resistance 2", 0),
        PerkImpl (0, "mage", "alteration", "Adept Alteration", "Cast Adept level Alteration spells for half magicka", "Apprentice Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Stability", "Alteration spells have greater duration", "Adept Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Expert Alteration", "Cast Expert level Alteration spells for half magicka", "Adept Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Atronach", "Absorb 30% of the magicka of any spells that hit you", "Expert Alteration", 0),
        PerkImpl (0, "mage", "alteration", "Master Alteration", "Cast Master level Alteration spells for half magicka", "Expert Alteration", 0),
        PerkImpl (0, "mage", "conjuration", "Novice Conjuration", "Cast novice level Conjuration spells for half magicka", "", 0),
        PerkImpl (0, "mage", "conjuration", "Conjuration Dual Casting", "Dual casting a Conjuration spell overcharges the effects into an even more powerful version", "Novice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Mystic Binding", "Bound weapons do more damage", "Novice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Apprentice Conjuration", "Cast Apprentice level Conjuration spells for half magicka", "Novice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Summoner 1", "Can summon atronachs or raise undead 2x as far away", "Novice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Summoner 2", "Can summon atronachs or raise undead 3x as far away", "Summoner 1", 0),
        PerkImpl (0, "mage", "conjuration", "Soul Stealer", "Bound weapons cast Soul Trap on targets", "Mystic Binding", 0),
        PerkImpl (0, "mage", "conjuration", "Necromancy", "Greater duration for reanimated undead", "Novice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Atromancy", "Double duration for conjured atronachs", "Summoner", 0),
        PerkImpl (0, "mage", "conjuration", "Oblivion Binding", "Bound weapons will banish summoned creatures and turn raised ones", "Soul Stealer", 0),
        PerkImpl (0, "mage", "conjuration", "Adept Conjuration", "Cast Adept level Conjuration spells for half magicka", "Apprentice Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Dark Souls", "Reanimated undead have 100 points more health", "Necromancy", 0),
        PerkImpl (0, "mage", "conjuration", "Expert Conjuration", "Cast Expert level Conjuration spells for half magicka", "Adept Conjuration", 0),
        PerkImpl (0, "mage", "conjuration", "Elemental Potency", "Conjured atronachs are 50% more powerful", "Atromancy", 0),
        PerkImpl (0, "mage", "conjuration", "Twin Souls", "You can have two atronachs or reanimated zombies [Elemental Potency]", "Elemental Potency", 0),
        PerkImpl (0, "mage", "conjuration", "Twin Souls", "You can have two atronachs or reanimated zombies [Dark Souls]", "Dark Souls", 0),
        PerkImpl (0, "mage", "conjuration", "Master Conjuration", "Cast Master level Conjuration spells for half magicka", "Expert Conjuration", 0),
        PerkImpl (0, "mage", "destruction", "Novice Destruction", "Cast Novice level Destruction spells for half magicka", "", 0),
        PerkImpl (0, "mage", "destruction", "Destruction Dual Casting", "Dual casting a Destruction spell overcharges the effects into an even more powerful version", "Novice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Apprentice Destruction", "Cast Apprentice level Destruction spells for half magicka", "Novice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Flames 1", "Fire spells do a total of 25% more than base damage", "Novice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Flames 2", "Fire spells do a total of 50% more than base damage", "Augmented Flames 1", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Frost 1", "Frost spells do a total of 25% more than base damage", "Novice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Frost 2", "Frost spells do a total of 50% more than base damage", "Augmented Frost 1", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Shock 1", "Shock spells do a total of 25% more than base damage", "Novice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Augmented Shock 2", "Shock spells do a total of 50% more than base damage", "Augmented Shock 1", 0),
        PerkImpl (0, "mage", "destruction", "Impact", "Most destruction spells will stagger an opponent when dual cast", "Destruction Dual Casting", 0),
        PerkImpl (0, "mage", "destruction", "Rune Master", "Can place runes five times farther away", "Apprentice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Adept Destruction", "Cast Adept level Destruction spells for half magicka", "Apprentice Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Intense Flames", "Fire damage causes targets to flee if their health is low", "Augmented Flames 1", 0),
        PerkImpl (0, "mage", "destruction", "Deep Freeze", "Frost damage paralyzes targets if their health is low", "Augmented Frost 1", 0),
        PerkImpl (0, "mage", "destruction", "Disintegrate", "Shock damage disintegrates targets if their health is low", "Augmented Shock 1", 0),
        PerkImpl (0, "mage", "destruction", "Expert Destruction", "Cast Expert level Destruction spells for half magicka", "Adept Destruction", 0),
        PerkImpl (0, "mage", "destruction", "Master Destruction", "Cast Master level Destruction spells for half magicka", "Expert Destruction", 0),
        PerkImpl (0, "mage", "illusion", "Novice Illusion", "Cast Novice level Illusion spells for half magicka", "", 0),
        PerkImpl (0, "mage", "illusion", "Illusion Dual Casting", "Dual casting an Illusion spell overcharges the effects into an even more powerful version", "Novice Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Animage", "Illusion spells now work on higher level animals", "Novice Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Apprentice Illusion", "Cast Apprentice level Illusion spells for half magicka", "Novice Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Hypnotic Gaze", "Calm spells now work on higher level opponents", "Novice Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Kindred Mage", "All Illusion spells work on higher level people", "Animage", 0),
        PerkImpl (0, "mage", "illusion", "Adept Illusion", "Cast Adept level Illusion spells for half magicka", "Apprentice Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Aspect of Terror", "Fear spells work on higher level opponents", "Hypnotic Gaze", 0),
        PerkImpl (0, "mage", "illusion", "Quiet Casting", "All spells you cast from any school of magic are silent to others", "Kindred Mage", 0),
        PerkImpl (0, "mage", "illusion", "Rage", "Frenzy spells work on higher level opponents", "Aspect of Terror", 0),
        PerkImpl (0, "mage", "illusion", "Expert Illusion", "Cast Expert level Illusion spells for half magicka", "Adept Illusion", 0),
        PerkImpl (0, "mage", "illusion", "Master of the Mind", "Illusion spells work on undead, daedra and automatons [Rage]", "Rage", 0),
        PerkImpl (0, "mage", "illusion", "Master of the Mind", "Illusion spells work on undead, daedra and automatons [Quiet Casting]", "Quiet Casting", 0),
        PerkImpl (0, "mage", "illusion", "Master Illusion", "Cast Master level Illusion spells for half magicka", "Expert Illusion", 0),
        PerkImpl (0, "mage", "restoration", "Novice Restoration", "Cast Novice level Restoration spells for half magicka", "", 0),
        PerkImpl (0, "mage", "restoration", "Restoration Dual Casting", "Dual casting a Restoration spell overcharges the effects into an even more powerful version", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Regeneration", "Healing spells cure 50% more", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Apprentice Restoration", "Cast Apprentice level Restoration spells for half magicka", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Recovery 1", "Magicka regenerates a total of 25% faster", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Recovery 2", "Magicka regenerates a total of 50% faster", "Recovery 1", 0),
        PerkImpl (0, "mage", "restoration", "Respite", "Healing spells also restore stamina", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Adept Restoration", "Cast Adept level Restoration spells for half magicka", "Apprentice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Ward Absorb", "Ward recharge your magicka when hit with spells", "Novice Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Necromage", "All spells are more effective against undead", "Regeneration", 0),
        PerkImpl (0, "mage", "restoration", "Expert Restoration", "Cast Expert level Restoration spells for half magicka", "Adept Restoration", 0),
        PerkImpl (0, "mage", "restoration", "Avoid Death", "Once a day, heals 250 points automatically if you fall below 10% health", "Recovery", 0),
        PerkImpl (0, "mage", "restoration", "Master Restoration", "Cast Master level Restoration spells for half magicka", "Expert Restoration", 0),
        PerkImpl (0, "mage", "enchanting", "Enchanter 1", "New enchantments are 20% stronger than base strength.", "", 0),
        PerkImpl (0, "mage", "enchanting", "Enchanter 2", "New enchantments are 40% stronger than base strength.", "Enchanter 1", 0),
        PerkImpl (0, "mage", "enchanting", "Enchanter 3", "New enchantments are 60% stronger than base strength.", "Enchanter 2", 0),
        PerkImpl (0, "mage", "enchanting", "Enchanter 4", "New enchantments are 80% stronger than base strength.", "Enchanter 3", 0),
        PerkImpl (0, "mage", "enchanting", "Enchanter 5", "New enchantments are 100% stronger than base strength.", "Enchanter 4", 0),
        PerkImpl (0, "mage", "enchanting", "Soul Squeezer", "Soul gems provide extra magicka for recharging.", "Enchanter 1", 0),
        PerkImpl (0, "mage", "enchanting", "Fire Enchanter", "Fire enchantments on weapons and armor are 25% stronger.  ", "Enchanter 1", 0),
        PerkImpl (0, "mage", "enchanting", "Soul Siphon", "Death blows to creatures, but not people, trap 5% of the victim's soul, recharging the weapon.  ", "Soul Squeezer", 0),
        PerkImpl (0, "mage", "enchanting", "Frost Enchanter", "Frost enchantments on weapons and armor are 25% stronger.  ", "Fire Enchanter", 0),
        PerkImpl (0, "mage", "enchanting", "Insightful Enchanter", "Skill enchantments on armor are 25% stronger.  ", "Enchanter 1", 0),
        PerkImpl (0, "mage", "enchanting", "Storm Enchanter", "Shock enchantments on weapons and armor are 25% stronger.  ", "Frost Enchanter", 0),
        PerkImpl (0, "mage", "enchanting", "Corpus Enchanter", "Helath, magicka, and stamina enchantments on armor are 25% stronger.  ", "Insightful Enchanter", 0),
        PerkImpl (0, "mage", "enchanting", "Extra Effect", "Can put two enchantments on the same item.  [Storm Enchanter]", "Storm Enchanter", 0),
        PerkImpl (0, "mage", "enchanting", "Extra Effect", "Can put two enchantments on the same item.  [Corpus Enchanter]", "Corpus Enchanter", 0),
        PerkImpl (0, "thief", "alchemy", "Alchemist 1", "Potions and poisons are a total of 20% stronger", "", 0),
        PerkImpl (0, "thief", "alchemy", "Alchemist 2", "Potions and poisons are a total of 40% stronger", "Alchemist 1", 0),
        PerkImpl (0, "thief", "alchemy", "Alchemist 3", "Potions and poisons are a total of 60% stronger", "Alchemist 2", 0),
        PerkImpl (0, "thief", "alchemy", "Alchemist 4", "Potions and poisons are a total of 80% stronger", "Alchemist 3", 0),
        PerkImpl (0, "thief", "alchemy", "Alchemist 5", "Potions and poisons are a total of 100% stronger", "Alchemist 4", 0),
        PerkImpl (0, "thief", "alchemy", "Physician", "Potions you mix that restore health, magicka or stamina are 25% more powerful", "Alchemist 1", 0),
        PerkImpl (0, "thief", "alchemy", "Poisoner", "Poisons you mix are 25% more effective", "Physician", 0),
        PerkImpl (0, "thief", "alchemy", "Benefactor", "Potions you mix with beneficial effects have an additional 25% greater magnitude", "Physician", 0),
        PerkImpl (0, "thief", "alchemy", "Experimenter 1", "Eating an ingredient reveals first two effects", "Benefactor", 0),
        PerkImpl (0, "thief", "alchemy", "Experimenter 2", "Eating an ingredient reveals first three effects", "Experimenter 1", 0),
        PerkImpl (0, "thief", "alchemy", "Experimenter 3", "Eating an ingredient reveals first four effects", "Experimenter 2", 0),
        PerkImpl (0, "thief", "alchemy", "Concentrated Poison", "Poisons applied to weapons last for twice as many hits", "Poisoner", 0),
        PerkImpl (0, "thief", "alchemy", "Green Thumb", "Two ingredients are gathered from plants", "Concentrated Poison", 0),
        PerkImpl (0, "thief", "alchemy", "Snakeblood", "50% resistance to all poisons [Concentrated Poison]", "Concentrated Poison", 0),
        PerkImpl (0, "thief", "alchemy", "Snakeblood", "50% resistance to all poisons [Experimenter]", "Experimenter", 0),
        PerkImpl (0, "thief", "alchemy", "Purity", "All negative effects are removed from created potions, and all positive effects are removed from created poisons", "Snakeblood", 0),
        PerkImpl (0, "thief", "light armor", "Agile Defender 1", "Light Armor has a total of 20% higher than base armor rating", "", 0),
        PerkImpl (0, "thief", "light armor", "Agile Defender 2", "Light Armor has a total of 40% higher than base armor rating", "Agile Defender 1", 0),
        PerkImpl (0, "thief", "light armor", "Agile Defender 3", "Light Armor has a total of 60% higher than base armor rating", "Agile Defender 2", 0),
        PerkImpl (0, "thief", "light armor", "Agile Defender 4", "Light Armor has a total of 80% higher than base armor rating", "Agile Defender 3", 0),
        PerkImpl (0, "thief", "light armor", "Agile Defender 5", "Light Armor has a total of 100% higher than base armor rating", "Agile Defender 4", 0),
        PerkImpl (0, "thief", "light armor", "Custom Fit ", "25% armor bonus if wearing all Light Armor: head, chest, hands, feet", "Agile Defender", 0),
        PerkImpl (0, "thief", "light armor", "Unhindered", "Light Armor weighs nothing and doesn't slow you down when worn", "Custom Fit", 0),
        PerkImpl (0, "thief", "light armor", "Wind Walker", "Stamina regenerates 50% faster in all Light Armor: head, chest, hands, feet", "Unhindered", 0),
        PerkImpl (0, "thief", "light armor", "Matching Set", "Additional 25% Armor bonus if wearing a matched set of Light Armor", "Custom Fit", 0),
        PerkImpl (0, "thief", "light armor", "Deft Movement", "10% of avoiding all damage from a melee attack while wearing all Light Armor: head, chest, hands, feet [Wind Walker]", "Wind Walker", 0),
        PerkImpl (0, "thief", "light armor", "Deft Movement", "10% of avoiding all damage from a melee attack while wearing all Light Armor: head, chest, hands, feet [Matching Set]", "Matching Set", 0),
        PerkImpl (0, "thief", "lockpicking", "Novice Locks", "Novice locks are much easier to pick", "", 0),
        PerkImpl (0, "thief", "lockpicking", "Apprentice Locks", "Apprentice locks are much easier to pick", "Novice Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Quick Hands", "Able to pick locks without being noticed", "Apprentice Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Wax Key", "Automatically gives you a copy of a picked lock's key if it has one", "Quick Hands", 0),
        PerkImpl (0, "thief", "lockpicking", "Adept Locks", "Adept locks are much easier to pick", "Apprentice Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Golden Touch", "Find more gold in chests", "Adept Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Treasure Hunter", "50% greater chance of finding special treasure", "Golden Touch", 0),
        PerkImpl (0, "thief", "lockpicking", "Expert Locks", "Expert locks are much easier to pick", "Adept Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Locksmith", "Pick starts close to the lock opening position", "Expert Locks", 0),
        PerkImpl (0, "thief", "lockpicking", "Unbreakable", "Lockpicks never break", "Locksmith", 0),
        PerkImpl (0, "thief", "lockpicking", "Master Locks", "Master locks are much easier to pick", "Expert Locks", 0),
        PerkImpl (0, "thief", "pickpocket", "Light Fingers 1", "Total Pickpocketing bonus of 20% over base chance. Item weight and value reduce pickpocketing odds", "", 0),
        PerkImpl (0, "thief", "pickpocket", "Light Fingers 2", "Total Pickpocketing bonus of 25% over base chance. Item weight and value reduce pickpocketing odds", "Light Fingers 1", 0),
        PerkImpl (0, "thief", "pickpocket", "Light Fingers 3", "Total Pickpocketing bonus of 30% over base chance. Item weight and value reduce pickpocketing odds", "Light Fingers 2", 0),
        PerkImpl (0, "thief", "pickpocket", "Light Fingers 4", "Total Pickpocketing bonus of 35% over base chance. Item weight and value reduce pickpocketing odds", "Light Fingers 3", 0),
        PerkImpl (0, "thief", "pickpocket", "Light Fingers 5", "Total Pickpocketing bonus of 40% over base chance. Item weight and value reduce pickpocketing odds", "Light Fingers 4", 0),
        PerkImpl (0, "thief", "pickpocket", "Night Thief", "+25% chance to pickpocket if the target is asleep", "Light Fingers 1", 0),
        PerkImpl (0, "thief", "pickpocket", "Poisoned", "Silently harm enemies by placing poisons in their pockets", "Night Thief", 0),
        PerkImpl (0, "thief", "pickpocket", "Cutpurse", "Pickpocketing gold is 50% easier", "Night Thief", 0),
        PerkImpl (0, "thief", "pickpocket", "Extra pockets", "Carrying capacity is increased by 100", "Night Thief", 0),
        PerkImpl (0, "thief", "pickpocket", "Keymaster", "Pickpocketing keys almost always works", "Cutpurse", 0),
        PerkImpl (0, "thief", "pickpocket", "Misdirection", "Can pickpocket equipped weapons", "Cutpurse", 0),
        PerkImpl (0, "thief", "pickpocket", "Perfect Touch", "Can pickpocket equipped items", "Misdirection", 0),
        PerkImpl (0, "thief", "sneak", "Stealth 1", "You are a total of 20% harder to detect when sneaking.", "", 0),
        PerkImpl (0, "thief", "sneak", "Stealth 2", "You are a total of 25% harder to detect when sneaking.", "Stealth 1", 0),
        PerkImpl (0, "thief", "sneak", "Stealth 3", "You are a total of 30% harder to detect when sneaking.", "Stealth 2", 0),
        PerkImpl (0, "thief", "sneak", "Stealth 4", "You are a total of 35% harder to detect when sneaking.", "Stealth 3", 0),
        PerkImpl (0, "thief", "sneak", "Stealth 5", "You are a total of 40% harder to detect when sneaking.", "Stealth 4", 0),
        PerkImpl (0, "thief", "sneak", "Muffled Movement", "Noise from armor is reduced by 50%.", "Stealth 1", 0),
        PerkImpl (0, "thief", "sneak", "Backstab", "Sneak attacks with one-handed weapons now do six times damage.", "Stealth 1", 0),
        PerkImpl (0, "thief", "sneak", "Light Foot", "You won't trigger pressure plates.", "Muffled Movement", 0),
        PerkImpl (0, "thief", "sneak", "Deadly Aim", "Sneak attacks with bows now do three times damage.", "Backstab", 0),
        PerkImpl (0, "thief", "sneak", "Silent Roll", "Sprinting while sneaking executes a silent forward roll.", "Light Foot", 0),
        PerkImpl (0, "thief", "sneak", "Assassin's Blade", "Sneak attacks with daggers now do a total of fifteen times normal damage.", "Deadly Aim", 0),
        PerkImpl (0, "thief", "sneak", "Silence", "Walking and running does not affect detection.", "Silent Roll", 0),
        PerkImpl (0, "thief", "sneak", "Shadow Warrior", "Crouching stops combat for a moment and forces distant opponents to search for a target.", "Silence", 0),
        PerkImpl (0, "thief", "speech", "Haggling 1", "Buying and selling prices are 10% better than base price.", "", 0),
        PerkImpl (0, "thief", "speech", "Haggling 2", "Buying and selling prices are 15% better than base price.", "Haggling 1", 0),
        PerkImpl (0, "thief", "speech", "Haggling 3", "Buying and selling prices are 20% better than base price.", "Haggling 2", 0),
        PerkImpl (0, "thief", "speech", "Haggling 4", "Buying and selling prices are 25% better than base price.", "Haggling 3", 0),
        PerkImpl (0, "thief", "speech", "Haggling 5", "Buying and selling prices are 30% better than base price.", "Haggling 4", 0),
        PerkImpl (0, "thief", "speech", "Allure", "10% better prices with the opposite sex", "Haggling 1", 0),
        PerkImpl (0, "thief", "speech", "Bribery", "Can bribe guards to ignore crimes", "Haggling 1", 0),
        PerkImpl (0, "thief", "speech", "Merchant", "Can sell any type of item to any kind of merchant", "Allure", 0),
        PerkImpl (0, "thief", "speech", "Persuasion", "Persuasion attempts are 30% easier", "Bribery", 0),
        PerkImpl (0, "thief", "speech", "Investor", "Can invest 500 gold with a shopkeeper to increase his available gold permanently", "Merchant", 0),
        PerkImpl (0, "thief", "speech", "Intimidation", "Intimidation is twice as successful", "Persuasion", 0),
        PerkImpl (0, "thief", "speech", "Fence", "Can barter stolen goods with any merchant you have invested in", "Investor", 0),
        PerkImpl (0, "thief", "speech", "Master Trader", "Every merchant in the world gains 1000 gold for bartering", "Fence", 0)]
        