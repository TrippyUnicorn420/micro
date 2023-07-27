-- Not working at all yet

-- Making a Text editor line type, with a string with all characters in the line, and a counter for how
-- many characters can still be put on this line
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
appendToLine line =
    let nextchar = getnewInput 
        in
            if (nextchar == '\ESC')
                then line
            else
                --getnewInput (appendChar nextchar line)
                appendToLine (appendChar nextchar line)

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar

appendChar :: IO Char -> TELine -> TELine
appendChar nextchar line = TELine ((contents line):nextchar) ((charspaceleft line) - 1)
