module Main where

import Network.HTTP


main :: IO ()
main =
  do
    putStrLn "Loading a word. This may take a couple of seconds."
    secret <- getRandomWord
    starman secret 8


starman :: String -> Int -> IO ()
starman secret =
  let
    display = replicate (length secret) '-'
  in
    mkTurn secret display


getRandomWord :: IO String
getRandomWord =
  let
    request = getRequest "http://setgetgo.com/randomword/get.php"
  in
    simpleHTTP request >>= getResponseBody


check :: String -> String -> Char -> (Bool, String)
check secret display guess =
  let
    exist = elem guess secret
    newDisplay = [ if guess == s then s else d | (s, d) <- zip secret display]
  in
    (exist, newDisplay)


mkTurn :: String -> String -> Int -> IO ()
mkTurn secret display turn
  | turn == 0 = putStrLn $ "You lose. It was: " ++ secret
  | display == secret = putStrLn $ secret ++ "! You win."
  | otherwise =  mkGuess secret display turn


mkGuess :: String -> String -> Int -> IO ()
mkGuess secret display turn =
  do putStrLn (display ++ "  " ++ replicate turn '*')
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check secret display (head q)
     let n' = if correct then turn else turn - 1
     mkTurn secret display' n'
