main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press the escape key.")
    let currentLine = ""
        in print (pure appendToLine currentLine)
    putStrLn ("Thank you for using Micro.")

appendToLine :: IO String -> IO String
appendToLine line =
    let nextchar = getnewInput 
        in
            if (nextchar == pure '\ESC')
                then line
            else
                --getnewInput (appendChar nextchar line)
                appendToLine (fmap appendChar nextchar line)

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar

appendChar :: Char -> String -> String
appendChar nextchar line = line ++ charToString nextchar

charToString :: Char -> String
charToString = (:[])