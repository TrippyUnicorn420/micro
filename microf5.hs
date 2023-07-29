import Text.Printf

-- Type representing text Editor
-- There are three strings that are concatenated when printed for user to view:
-- stringBeforeCursor is all characters before the current cursor position, cursor is the | character
-- and stringAfterCursor is all characters after the current cursor position
-- charspaceleft : used to keep track of how many more characters can be put in before a newline character is required 
data TELine = TELine 
    { stringBeforeCursor :: String
    , cursor :: String
    , stringAfterCursor :: String
    } deriving Show

data TextFormat = TextFormat
    { currWord :: String
    , currWordLength :: Integer
    , textContents :: String
    , currTextLength :: Integer
    , textToFormat :: String
    } deriving Show

main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press Ctrl+X. To save, please press Ctrl+S.")
    -- Intital Text editor, with only cursor at the start
    let currentLine = TELine "" "|" ""
        in do 
            printEditorView currentLine
            editorView <- appendToLine (pure currentLine)
            printEditorView editorView
    putStrLn ("\n" ++ "Thank you for using Micro.")

-- Prints the current contents of the TELine that user should see (strings only)
printEditorView :: TELine -> IO()
printEditorView editorView = do
    putStr("\n")
    let view = (formatTELine editorView) in
    -- Any new ideas? I can't work out how to download System.Console.ANSI
        putStrLn (view)

formatTELine :: TELine -> String
formatTELine editorView = textContents (formatting (TextFormat "" 0 "" 0 (stringBeforeCursor editorView ++ cursor editorView ++ stringAfterCursor editorView)))

-- Need formatting function:
formatting :: TextFormat -> TextFormat
formatting te = do
    let word = (currWord te)
    let wLength = (currWordLength te)
    let formatted = (textContents te)
    let fLength = (currTextLength te)
    let toFormat = (textToFormat te)
    -- if there is still stuff to format
    if (toFormat /= "") then do
        -- Get next character
        let newChar = take 1 toFormat
        let newWLength = wLength + 1
        -- If a space, it means the word is finished
        if (newChar == " ") then
            if (fLength + newWLength > 121) then
                formatting (TextFormat "" 0 (formatted ++ word ++ "\n") 0 (tail toFormat))
            else
                formatting (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat))
        else if (newWLength == 120) then
            --add the current portion of the word, and a new line character
            formatting (TextFormat "" 0 (formatted ++ word ++ newChar ++ "\n") 0 (tail toFormat))
        else if (fLength + newWLength > 120) then
            --add newline to the printText, keep word the same
            formatting (TextFormat (word ++ newChar) newWLength (formatted ++ "\n") 0 (tail toFormat))
        else
            -- add new character to word, keep going
            formatting (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat))
    else
        TextFormat "" 0 (formatted ++ word) fLength toFormat

appendToLine :: IO TELine -> IO TELine
appendToLine line =
    line
    >>= \line ->
        getnewInput
        >>= \nextchar ->
            if (nextchar == '\^X')
                then pure line
            else 
                if (nextchar == '\n') then
                    do
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\DEL') then -- for backspace
                    do
                        newline <- pure (TELine (shave (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\ESC') then
                    do
                        getnewInput
                        >>= \bracketchar ->
                            if (bracketchar == '[')
                                then do
                                    getnewInput
                                    >>= \numberchar ->
                                        if (numberchar == '3') then
                                            do
                                                getnewInput
                                                >>= \squigglechar ->
                                                    if (squigglechar == '~') then
                                                        do
                                                            newline <- pure (TELine (stringBeforeCursor line) (cursor line) (tail (stringAfterCursor line)))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                                    else
                                                        do
                                                            newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                        else
                                            do
                                                newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                                                printEditorView newline
                                                appendToLine (pure newline)
                            else
                                do
                                    newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                                    printEditorView newline
                                    appendToLine (pure newline)
                else
                    do
                    --appendToLine (pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) ((charspaceleft line) - 1)))
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line))
                        printEditorView newline
                        appendToLine (pure newline)

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar

getControlInput :: IO String
getControlInput = do
    nextstr <- getLine
    return nextstr

appendNewLine :: String -> String -> String
appendNewLine newlinestr line = line ++ newlinestr

appendChar :: Char -> String -> String
appendChar nextchar line = line ++ charToString nextchar

charToString :: Char -> String
charToString = (:[])

-- trying to make backspace work
shave :: [a] -> [a]
shave [] = []
shave [h]    = []
shave (h:t)  =[h]++shave t