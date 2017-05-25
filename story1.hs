import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String
type Hero = String

hero :: Hero
hero = "Colette"

-- (Where you are, if you go to this direction), where you will be)
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("voleur", "b"), "cave"),
    (("cave", "h"), "voleur"),
    (("cave", "o"), "cave entrance"),
    (("cave entrance", "e"), "cave"),
    (("cave entrance", "s"), "meadow"),
    (("meadow", "s"), "building"),
    (("meadow", "n"), "cave entrance"),
    (("building", "o"), "cage"),
    (("building", "n"), "meadow"),
    (("building", "e"), "closet"),
    (("cage", "e"), "building"),
    (("closet", "o"), "building")
    ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("ruby", "voleur"),
    ("key", "cave entrance"),
    ("flashlight", "building"),
    ("sword", "closet"),
    ("hero", "meadow"),
    -- This is a hack, so I don't have to add more lists to the "World" state
    ("voleur", "alive")
    ]

type World = (PathMap, LocationMap, Response, Hero)
world :: IO (PathMap, LocationMap, Response, Hero)
world = return (paths, locations, "", hero)

main :: IO (String)
main = do
    putStrLn "\nVoici le jeu d'aventure de Colette!\n"
    putStrLn instructions
    play_game $ return (paths, locations, "", hero)
    return "Au revoir!"

instructions =
    "Utilisez un ou deux mots pour commander le jeu.\n" ++
    "Les commandes disponibles sont:\n" ++
    "main               -- pour démmarer le jeu.\n" ++
    "name nom           -- pour chaner le nom du héro.\n" ++
    "n  s  e  o  h  b   -- aller dans cette direction.\n" ++
    "take object        -- pour prendre l'objet avec son nom.\n" ++
    "drop object        -- pour réposer l'objet avec son nom.\n" ++
    "kill               -- pour attaquer un ennemis.\n" ++
    "look               -- pour regarder autours.\n" ++
    "i                  -- pour connaitre l'inventaire que vous transportez.\n" ++
    "quit               -- pour quitter le jeu."

play_game :: IO (World) -> IO (World)
play_game world = do
    (paths, locations, response, hero) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "", hero)
        else do
            putStr (hero ++ "> ")
            command <- getLine
            if command == "quit"
                then return (paths, locations, "Quitting.", hero)
                else  play_game $ return (do_command command paths locations)

game_over :: LocationMap -> Bool
game_over locations =
    let my_location = get "hero" locations
        ruby_location = get "ruby" locations
    in my_location == "dead" || (my_location == "meadow" && ruby_location == "holding")

can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "meadow" "n" _ locations= get "flashlight" locations == "holding"
can_move "building" "e" _ locations = get "key" locations == "holding"
can_move from direction paths _ =
    elem (from, direction) keys
    where (keys, _) = unzip paths

cannot_move_because :: Location -> Direction -> Response
cannot_move_because "meadow" "n" = "Go into that dark cave without a light? Are you crazy?"
cannot_move_because "building" "e" = "The door appears to be locked."
cannot_move_because _ _ = "You can't go that way."

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "o" paths locations = go "o" paths locations
do_command "h" paths locations = go "h" paths locations
do_command "b" paths locations = down_from_spider "b" paths locations
do_command "look" paths locations = look paths locations
do_command "kill" paths locations = kill paths locations
do_command "i" paths locations = (paths, locations, inventory locations, hero)
do_command "quit" paths locations = (paths, locations, "quit", hero)
do_command "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations, hero)
do_command cmd paths locations = do_command_2 cmd paths locations

do_command_2 :: String -> PathMap -> LocationMap -> World
do_command_2 cmd paths locations
    | isPrefixOf "take " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "name " cmd =
          game_name (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "Je ne comprend pas: " ++ cmd, hero)

game_take :: Thing -> PathMap -> LocationMap -> World
game_take thing paths locations =
    let here = get "hero" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "holding" locations), "OK, taken.", hero)
       else if there == "holding"
            then (paths, locations, "You are already holding it.", hero)
            else (paths, locations, "I don't see it here.", hero)

game_drop :: Thing -> PathMap -> LocationMap -> World
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "hero" locations
        there = get thing locations
    in if there == "holding"
        then (paths, (put thing here locations), "OK, dropped.", hero)
        else (paths, locations, "You aren't holding it.", hero)

game_name :: String -> PathMap -> LocationMap -> World
game_name newname paths locations = (paths, locations, "Enchanté "++newname, newname)

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let my_location = get "hero" locations
    if can_move my_location direction paths locations
        then do
            let new_location = move my_location direction paths
            let new_locations = put "hero" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response, hero)
        else (paths, locations, cannot_move_because my_location direction, hero)

-- Check if you go down from the voleur with the ruby but without killing him -> you are dead
down_from_spider :: String -> PathMap -> LocationMap -> World
down_from_spider direction paths locations =
    if get "hero" locations == "voleur" &&
       get "voleur" locations == "alive" &&
       get "ruby" locations == "holding"
           then (paths, put "hero" "dead" locations, description "cave3", hero)
           else go direction paths locations

look :: PathMap -> LocationMap -> World
look paths locations =
    if things == []
        then (paths, locations, describe my_location locations, hero)
        else (paths, locations, describe my_location locations ++ "\n\n" ++ things, hero)
    where my_location = get "hero" locations
          things = items_here locations

kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "hero" locations of
        "cage" -> (paths,
                   put "hero" "dead" locations,
                   "Oh, bad idea! You have just been eaten by a lion.", hero)
        "cave" -> (paths, locations,
                   "The spider's leg is about as tough as a telephone pole.", hero)
        "voleur" ->
            if get "sword" locations == "holding"
                then (paths,
                      put "voleur" "dead" locations,
                      "You hack repeatedly at the spider's back.  Slimy ichor\n" ++
                     "gushes out of the spider''s back, and gets all over you.\n" ++
                     "I think you have killed it, despite the continued twitching.", hero)
                else (paths,
                      locations,
                      "Beating on the spider's back with your fists has no\n" ++
                      "effect.  This is probably just as well.", hero)
        _ -> (paths, locations, "I see nothing inimical here.", hero)

inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "holding") <- locations]
    in if my_stuff == []
        then "You aren't holding anything."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "hero" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "hero"]
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "hero" locations
        spider_status = get "voleur" locations
        ruby_location = get "ruby" locations
    in describe_helper here spider_status ruby_location  locations

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "meadow" "dead" "holding" locations = description "meadow2"
describe_helper "cave" "alive" "holding" locations = description "cave3"
describe_helper "cave" "dead" _ locations = description "cave2"
describe_helper "voleur" "dead" _ locations = description "spider2"
describe_helper here _ _ locations = description here

description :: Location -> String
description "meadow" =
    "You are in a meadow.  To the north is the dark mouth\n" ++
    "of a cave; to the south is a small building.  Your\n" ++
    "assignment, should you decide to accept it, is to\n" ++
    "recover the famed Bar-Abzad ruby and return it to\n" ++
    "this meadow."

description "meadow2" = "Congratulations!!  You have recovered the ruby and won the game."

description "building" =
    "You are in a small building.  The exit is to the north.\n" ++
    "There is a barred door to the west, but it seems to be\n" ++
    "unlocked.  There is a smaller door to the east."

description "cage" =
    "You are in a lion's den!  The lion has a lean and\n" ++
    "hungry look.  You better get out of here!"

description "closet" =
    "This is nothing but an old storage closet."

description "cave entrance" =
    "You are in the mouth of a dank cave.  The exit is to\n" ++
    "the south; there is a large, dark, round passage to\n" ++
    "the east."

description "cave" =
    "There is a giant spider here!  One hairy leg, about the\n" ++
    "size of a telephone pole, is directly in front of you!\n" ++
    "I would advise you to leave promptly and quietly...."

description "cave2" =
    "Yecch!  There is a giant spider here, twitching."

description "cave3" =
     "The spider sees you with the ruby and attacks!!!\n" ++
     "    ...it is over in seconds...."

description "voleur" =
    "You are on top of a giant spider, standing in a rough\n" ++
    "mat of coarse hair.  The smell is awful."

description "spider2" =
    "Oh, gross!  You''re on top of a giant dead spider!"

description someplace = someplace ++ ", and you can't see anything."
