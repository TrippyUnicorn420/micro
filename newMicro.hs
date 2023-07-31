import Text.Printf

-- Type representing text Editor
-- There are three strings that are concatenated when printed for user to view:
-- stringBeforeCursor is all characters before the current cursor position, cursor is the | character
-- and stringAfterCursor is all characters after the current cursor position
-- charspaceleft : used to keep track of how many more characters can be put in before a newline character is required 
data TELine = TELine 
    { stringBeforeCursor :: String
    , cursorPos :: Integer
    , stringAfterCursor :: String
    } deriving Show

data TextFormat = TextFormat
    { currWord :: String
    , currWordLength :: Integer
    , textContents :: String
    , currTextLength :: Integer
    , textToFormat :: String
    , totalCharsParsed :: Integer
    , currCursorPos :: Integer
    , cursorToPrint :: Bool
    } deriving Show

main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press Ctrl+X. To save, please press Ctrl+S.")
    -- Intital Text editor, with only cursor at the start
    let currentLine = TELine "" 0 ""
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
        putStrLn (view)

formatTELine :: TELine -> String
formatTELine editorView = textContents (formatting (TextFormat "" 0 "" 0 (stringBeforeCursor editorView ++ stringAfterCursor editorView) 0 (cursorPos editorView) True))

-- Need formatting function:
formatting :: TextFormat -> TextFormat
formatting te = do
    let word = (currWord te)
    let wLength = (currWordLength te)
    let formatted = (textContents te)
    let fLength = (currTextLength te)
    let toFormat = (textToFormat te)
    let totalParsed = (totalCharsParsed te)
    let cursor = (currCursorPos te)
    let isNotPrinted = (cursorToPrint te)
    if (totalParsed == cursor && isNotPrinted) then
        formatting (TextFormat (word ++ "|") wLength formatted fLength toFormat totalParsed cursor False)
    else
        -- if there is still stuff to format
        if (toFormat /= "") then do
            -- Get next character
            let newChar = take 1 toFormat
            let newWLength = wLength + 1
            -- If a space, it means the word is finished
            if (newChar == " ") then
                if (fLength + newWLength > 120) then
                    formatting (TextFormat "" 0 (formatted ++ word ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted)
                else
                    formatting (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor isNotPrinted)
            else if (newWLength == 120) then
                --add the current portion of the word, and a new line character
                formatting (TextFormat "" 0 (formatted ++ word ++ newChar ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted)
            else if (fLength + newWLength > 120) then
                --add newline to the printText, keep word the same
                formatting (TextFormat (word ++ newChar) newWLength (formatted ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted)
            else
                -- add new character to word, keep going
                formatting (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor isNotPrinted)
        else
            TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor isNotPrinted


wordLengths :: [String] -> [Int]
wordLengths = map length

removeFirst :: [a] -> [a]
removeFirst = \myList ->
    case myList of
        [] -> []
        x:xs -> xs

plusOne :: Int -> Int
plusOne x = x + 1

-- Returns line number of cursor in current line
getCurrLine :: Int -> [Int] -> Int
getCurrLine currentPos lineLengths = checkLineTotals 0 currentPos 0 lineLengths

--Returns number of characters before cursor in current line
getCurrColumn :: Int -> Int -> [Int] -> Int
getCurrColumn currentPos currLine lineLengths = currentPos - (getTotals 0 (currLine - 1) lineLengths)

-- Finds total number of characters in specified number of lines
getTotals :: Int -> Int -> [Int] -> Int
getTotals total line listLines =
    -- If line is negative (i.e. case where we are going from line 1 to line 0)
    if (line < 0) then
        total
    else
        let newTotal = (total + (listLines!!0)) in
            if (line == 0) then
                newTotal
            else
                getTotals newTotal (line - 1) (removeFirst listLines)

-- Finds line at which cursor is found
checkLineTotals :: Int -> Int -> Int -> [Int] -> Int
checkLineTotals total cursorPos line listLines =
    let newTotal = (total + (listLines!!0)) in
        if (newTotal >= cursorPos) then
            line
        else if (listLines == []) then
            line
        else
            checkLineTotals newTotal cursorPos (line + 1) (removeFirst listLines)

-- Returns number of characters before cursor a line up
-- E.g. upCursor 2 50 [90, 20, 80] returns (90 + min (50, 20)) = 110
upCursorPos :: Int -> Int -> [Int] -> Int
upCursorPos line column listLines = (getTotals 0 (line - 2) listLines) + (min column (listLines!!(line - 1) - 1))

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
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) ((cursorPos line) + 1) (stringAfterCursor line))
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\DEL') then -- for backspace
                    do
                        newline <- pure (TELine (shave (stringBeforeCursor line)) (checkCursorDelete (cursorPos line)) (stringAfterCursor line))
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
                                                            newline <- pure (TELine (stringBeforeCursor line) (cursorPos line) (tail (stringAfterCursor line)))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                                    else
                                                        do
                                                            newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) ((cursorPos line) + 1) (stringAfterCursor line))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                        -- IF up arrow is pressed
                                        else if (numberchar == 'A') then
                                            do
                                                let oldCursorPos = fromIntegral (cursorPos line)
                                                -- Get formatted string
                                                let oldString = textContents (formatting (TextFormat "" 0 "" 0 (stringBeforeCursor line ++ stringAfterCursor line) 0 (cursorPos line) True))
                                                -- Turn that into list of integers, each element being the length of each string
                                                let listLen = map plusOne (wordLengths (lines oldString))
                                                -- Find the line that cursor is currently on
                                                let currLine = getCurrLine oldCursorPos listLen
                                                -- If line 0, don't do anything
                                                if (currLine == 0) then do
                                                    newline <- pure line
                                                    printEditorView newline
                                                    appendToLine (pure newline)
                                                -- Else move up a line
                                                else do
                                                    -- Find column that cursor is currently on
                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                    -- Get new cursor position
                                                    let newCursorPos = upCursorPos currLine currColumn listLen
                                                    -- Find number of characters to move to stringAfterCursor
                                                    let moveN = oldCursorPos - newCursorPos
                                                    let stringToMove = reverse (take moveN (reverse (stringBeforeCursor line)))
                                                    newline <- pure (TELine (take newCursorPos (stringBeforeCursor line)) (toInteger newCursorPos) (stringToMove ++ (stringAfterCursor line)))
                                                    printEditorView newline
                                                    appendToLine (pure newline)
                                        else
                                            do
                                                newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) ((cursorPos line) + 1) (stringAfterCursor line))
                                                printEditorView newline
                                                appendToLine (pure newline)
                            else
                                do
                                    newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) ((cursorPos line) + 1) (stringAfterCursor line))
                                    printEditorView newline
                                    appendToLine (pure newline)
                else
                    do
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) ((cursorPos line) + 1) (stringAfterCursor line))
                        printEditorView newline
                        appendToLine (pure newline)

checkCursorDelete :: Integer -> Integer
checkCursorDelete cursor = 
    if (cursor == 0) then
        cursor
    else
        cursor - 1

getnewInput :: IO Char
getnewInput = do 
    nextchar <- getChar
    return nextchar

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