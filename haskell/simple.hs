import Data.Char (isSpace)

startsWith :: String -> String -> Bool
startsWith x "" = True
startsWith (x : xs) (y : ys) | x == y = startsWith xs ys
startsWith _ _ = False

lstrip :: String -> String -> String
lstrip (x : xs) (y : ys) | x == y = lstrip xs ys
lstrip x _ = x

-- Stupid but simple.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

prog :: String -> String
prog = handleInput . trim where
  handleInput "Hello." = "Hello, what is your name?"
  handleInput x | startsWith x "My name is " = "Hello, " ++ (lstrip x "My name is ") ++ " Nice to meet you."
  handleInput x = "I don't understand " ++ x

interactByLines :: (String -> String) -> (IO ())
interactByLines f = do
  input <- getLine
  putStrLn $ f input
  interactByLines f

main = interactByLines prog
