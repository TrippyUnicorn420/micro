main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press the escape key.")
    let currentLine = ""
        in appendToLine (pure currentLine) >>= print
    putStrLn ("Thank you for using Micro.")

appendToLine :: IO String -> IO String
appendToLine line =
    line
    >>= \line ->
        getnewInput
        >>= \nextchar ->
            if (nextchar == '\ESC')
                then pure line
            else
                --getnewInput (appendChar nextchar line)
                appendToLine (pure (appendChar nextchar line))

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar

appendChar :: Char -> String -> String
appendChar nextchar line = line ++ charToString nextchar

charToString :: Char -> String
charToString = (:[])