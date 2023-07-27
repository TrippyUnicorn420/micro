data TELine = TELine 
    { contents :: String
    , charspaceleft :: Integer
    } deriving Show

main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press the escape key.")
    let currentLine = TELine "" 120
        in print (appendToLine currentLine)
    putStrLn ("Thank you for using Micro.")


appendToLine :: TELine -> TELine
appendToLine line = do
    nextchar <- getChar
    if (nextchar == '\ESC')
        then return line
    else
        appendToLine (TELine ((contents line) ++ (charToString nextchar)) ((charspaceleft line) - 1))

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar


charToString :: Char -> String
charToString = (:[])