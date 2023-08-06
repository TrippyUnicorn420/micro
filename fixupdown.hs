{-# LANGUAGE BlockArguments #-}
import Text.Printf
import System.IO
import Control.Exception


data TELine = TELine
    { contents :: String
    , cursorPos :: Int
    , charTotal :: Int
    , leftBrackPos :: Int
    , rightBrackPos :: Int
    , pivotHighlight :: Int
    } deriving Show

data TextFormat = TextFormat
    { currWord :: String
    , currWordLength :: Int
    , textContents :: String
    , currTextLength :: Int
    , textToFormat :: String
    , totalCharsParsed :: Int
    , currCursorPos :: Int
    , cursorToPrint :: Bool
    , lbPos :: Int
    , lbToPrint :: Bool
    , rbPos :: Int
    , rbToPrint :: Bool
    } deriving Show

main :: IO()
main = do
    putStrLn ("Welcome to Micro.\nCtrl+O: open a file\tCtrl+S: save\tCtrl+X: exit without saving")
    -- Intital Text editor, with only cursor at the start
    let lol = (TELine "" 0 0 0 0 0): []
    let currentLine = head lol
        in do
            printEditorView currentLine
            editorView <- appendToLine (pure currentLine)
            printEditorView editorView
    putStrLn ("\n" ++ "Thank you for using Micro.")

-- Prints the current contents of the TELine that user should see (strings only)
printEditorView :: TELine -> IO()
printEditorView editorView = do
    putStr ("\n")
    let view = formatTELine editorView
    printf ("\ESC[2J\ESC[H\ESC[0;0f")
    putStrLn (view)

checkingPrintTELine :: TELine -> IO()
checkingPrintTELine editorView = do
    putStr ("\n")
    print editorView

-- checkingPrintFormat :: TELine -> IO()
-- checkingPrintFormat editorView = do
--     putStr ("\n")
--     print (formatting (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))

formatTELine :: TELine -> String
formatTELine editorView = textContents (formatting (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))

formatForSave :: TELine -> String
formatForSave editorView = textContents (saveFormat (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))


-- Need formatting function:
-- Assuming 'lol' is a list of TELine elements outside the scope of the function
formatting :: [TELine] -> TextFormat -> ([TELine], TextFormat)
formatting lol te = do
    let word = currWord te
    let wLength = currWordLength te
    let formatted = textContents te
    let fLength = currTextLength te
    let toFormat = textToFormat te
    let totalParsed = totalCharsParsed te
    let cursor = currCursorPos te
    let isNotPrinted = cursorToPrint te
    let lb = lbPos te
    let lbNotPrinted = lbToPrint te
    let rb = rbPos te
    let rbNotPrinted = rbToPrint te
    if (totalParsed == lb && lb /= rb && lbNotPrinted) then
        formatting lol (TextFormat (word ++ "[") wLength formatted fLength toFormat totalParsed cursor isNotPrinted lb False rb rbNotPrinted)
    else if (totalParsed == rb && lb /= rb && rbNotPrinted) then
        formatting lol (TextFormat (word ++ "]") wLength formatted fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb False)
    else if (totalParsed == cursor && isNotPrinted) then
        formatting lol (TextFormat (word ++ "|") wLength formatted fLength toFormat totalParsed cursor False lb lbNotPrinted rb rbNotPrinted)
    else
        -- if there is still stuff to format
        if (toFormat /= "") then do
            -- Get next character
            let newChar = take 1 toFormat
            let newWLength = wLength + 1

            -- If a space, it means the word is finished
            if (newChar == " ") then
                if (fLength + newWLength >= 120) then
                    formatting (lol ++ [TELine (formatted ++ word ++ "\n") 0 (fLength + newWLength) 0 0 0]) (TextFormat "" 0 "" 0 (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
                else
                    formatting lol (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
            else if (newChar == "\n") then
                formatting lol (TextFormat "" 0 (formatted ++ word ++ newChar) 0 (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
            else if (newWLength == 121) then
                -- slight issue with out by one error here with up arrow, but probably can leave it
                --add the current portion of the word, and a new line character
                formatting (lol ++ [TELine (formatted ++ (tail (word)) ++ "\n") 0 1 0 0 0]) (TextFormat newChar 1 "" 0 (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
            else if (fLength + newWLength > 120) then
                --add newline to the printText, keep word the same
                formatting (lol ++ [TELine (formatted ++ "\n") 0 0 0 0 0]) (TextFormat (word ++ newChar) newWLength "" 0 (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
            else
                -- add new character to word, keep going
                formatting lol (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor True lb lbNotPrinted rb rbNotPrinted)
        else
            (lol, TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor True lb lbNotPrinted rb rbNotPrinted)




saveFormat :: TextFormat -> TextFormat
saveFormat te = do
    let word = (currWord te)
    let wLength = (currWordLength te)
    let formatted = (textContents te)
    let fLength = (currTextLength te)
    let toFormat = (textToFormat te)
    let totalParsed = (totalCharsParsed te)
    let cursor = (currCursorPos te)
    let isNotPrinted = (cursorToPrint te)
    let lb = (lbPos te)
    let lbNotPrinted = (lbToPrint te)
    let rb = (rbPos te)
    let rbNotPrinted = (rbToPrint te)
    if (toFormat /= "") then do
        -- Get next character
        let newChar = take 1 toFormat
        let newWLength = wLength + 1
        -- If a space, it means the word is finished
        if (newChar == " ") then
            saveFormat (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else if (newChar == "\n") then
            saveFormat (TextFormat "" 0 (formatted ++ word ++ newChar) 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else
            -- add new character to word, keep going
            saveFormat (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
    else
        TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted

upDownFormatting :: TextFormat -> TextFormat
upDownFormatting te = do
    let word = (currWord te)
    let wLength = (currWordLength te)
    let formatted = (textContents te)
    let fLength = (currTextLength te)
    let toFormat = (textToFormat te)
    let totalParsed = (totalCharsParsed te)
    let cursor = (currCursorPos te)
    let isNotPrinted = (cursorToPrint te)
    let lb = (lbPos te)
    let lbNotPrinted = (lbToPrint te)
    let rb = (rbPos te)
    let rbNotPrinted = (rbToPrint te)
    -- if there is still stuff to format
    if (toFormat /= "") then do
        -- Get next character
        let newChar = take 1 toFormat
        let newWLength = wLength + 1
        -- If a space, it means the word is finished
        if (newChar == " ") then
            if (fLength + newWLength >= 120) then
                upDownFormatting (TextFormat "" 0 (formatted ++ word ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            else
                upDownFormatting (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else if (newChar == "\n") then
            if (toFormat /= "\n") then
                upDownFormatting (TextFormat "" 0 (formatted ++ word ++ newChar) 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            -- catching special case of ending in a newline (makes the line 1 long)
            else
                upDownFormatting (TextFormat "a" 1 (formatted ++ word ++ newChar) 1 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else if (newWLength == 121) then
            -- slight issue with out by one error here with up arrow, but probably can leave it
            --add the current portion of the word, and a new line character
            upDownFormatting (TextFormat newChar 1 (formatted ++ (tail (word)) ++ "\n") 1 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else if (fLength + newWLength > 120) then
            --add newline to the printText, keep word the same
            upDownFormatting (TextFormat (word ++ newChar) newWLength (formatted ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else
            -- add new character to word, keep going
            upDownFormatting (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
    else
        TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted

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
    -- If line is negative (i.e. case where we are going from line 1 to line)
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
        if (listLines == []) then
            line
        else if (newTotal > cursorPos) then
            line
        else
            checkLineTotals newTotal cursorPos (line + 1) (removeFirst listLines)

-- Will split string to before and after cursor, and put char where cursor is
addChar :: Char -> String -> Int -> Int -> String
addChar toAdd text cursorPos totalChar = (take cursorPos text) ++ (charToString toAdd) ++ (reverse (take (totalChar - cursorPos) (reverse text)))

removeCharLeft :: String -> Int -> Int -> String
removeCharLeft text cursorPos totalChar = (init (take cursorPos text)) ++ (reverse (take (totalChar - cursorPos) (reverse text)))

removeCharRight :: String -> Int -> Int -> String
removeCharRight text cursorPos totalChar = (take cursorPos text) ++ (tail (reverse (take (totalChar - cursorPos) (reverse text))))

deleteHighlight :: String -> Int -> Int -> Int -> String
deleteHighlight text lbPos rbPos totalChar = (take lbPos text) ++ (reverse (take (totalChar - rbPos) (reverse text)))

replaceHighlight :: Char -> String -> Int -> Int -> Int -> String
replaceHighlight toAdd text lbPos rbPos totalChar = (take lbPos text) ++ (charToString toAdd) ++ (reverse (take (totalChar - rbPos) (reverse text)))

-- Returns number of characters before cursor a line up
-- E.g. upCursorPos 2 50 [90, 20, 80] returns (90 + min (50, 20)) = 110
upCursorPos :: Int -> Int -> [Int] -> Int
upCursorPos line column listLines = (getTotals 0 (line - 2) listLines) + (min column (listLines!!(line - 1) - 1))

-- Returns number of characters after cursor a line down
-- E.g. downCursorPos 0 3 [6, 6, 5] returns (6 + min (50, 20)) = 110
downCursorPos :: Int -> Int -> [Int] -> Int
downCursorPos line column listLines = ((getTotals 0 line listLines) + (min column (listLines!!(line + 1) - 1)))

appendToLine :: IO TELine -> IO TELine
appendToLine line =
    line
    >>= \line ->
        getnewInput
        >>= \nextchar ->
            if (nextchar == '\^X')
                then pure line
            else if (nextchar == '\^W') then
                do
                    save (formatForSave line)
                    pure line
            else if (nextchar == '\^O') then
                do
                    newline <- load
                    printEditorView newline
                    appendToLine (pure newline)
            else
                if (nextchar == '\n') then
                    -- If text is highlighted, replace it with this character
                    if (leftBrackPos line /= rightBrackPos line) then do
                        newline <- pure (TELine (replaceHighlight nextchar (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) ((leftBrackPos line)+1) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line)) + 1) ((leftBrackPos line)+1) ((leftBrackPos line)+1) ((leftBrackPos line)+1))
                        printEditorView newline
                        appendToLine (pure newline)
                    else do
                        newline <- pure (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))
                        --checkingPrintFormat(newline)
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\DEL') then -- for backspace
                    -- If text is highlighted, delete the highlighted section
                    if (leftBrackPos line /= rightBrackPos line) then do
                        newline <- pure (TELine (deleteHighlight (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) (leftBrackPos line) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line))) (leftBrackPos line) (leftBrackPos line) (leftBrackPos line))
                        printEditorView newline
                        appendToLine (pure newline)
                    else if ((cursorPos line) == 0) then do
                        newline <- pure line
                        printEditorView newline
                        appendToLine (pure newline)
                    else do
                        newline <- pure (TELine (removeCharLeft (contents line) (cursorPos line) (charTotal line)) (checkCursorDelete (cursorPos line)) ((charTotal line) - 1) (checkCursorDelete (cursorPos line)) (checkCursorDelete (cursorPos line)) (checkCursorDelete (cursorPos line)))
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
                                                    -- If delete key is pressed
                                                    if (squigglechar == '~') then
                                                        -- If there is text highlighted, remove it
                                                        if (leftBrackPos line /= rightBrackPos line) then do
                                                            newline <- pure (TELine (deleteHighlight (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) (leftBrackPos line) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line))) (leftBrackPos line) (leftBrackPos line) (leftBrackPos line))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                                        -- If nothing after cursor, don't do anything
                                                        else if ((cursorPos line) == (charTotal line)) then do
                                                            newline <- pure line
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                                        -- If something after cursor, delete first char after the cursor
                                                        else do
                                                            newline <- pure (TELine (removeCharRight (contents line) (cursorPos line) (charTotal line)) (cursorPos line) ((charTotal line) - 1) (cursorPos line) (cursorPos line) (cursorPos line))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                                    -- Weird keypress - don't do anything
                                                    else
                                                        do
                                                            newline <- pure (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) (cursorPos line) (cursorPos line) (cursorPos line))
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                        else if (numberchar == '1') then
                                            do
                                                getnewInput
                                                >>= \semichar ->
                                                    if (semichar == ';') then
                                                        do
                                                            getnewInput
                                                            >>= \twochar ->
                                                                if (twochar == '2') then
                                                                    do
                                                                        getnewInput
                                                                        >>= \shiftarrow ->
                                                                            -- Shift right arrow
                                                                            if (shiftarrow == 'C') then
                                                                                do
                                                                                    -- If there is nothing more to the right, do nothing
                                                                                    if ((cursorPos line) == (charTotal line)) then do
                                                                                        newline <- pure line
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If highlight is getting larger ([]| to [.....]|)
                                                                                    else if ((pivotHighlight line) == (leftBrackPos line) && (pivotHighlight line) == (rightBrackPos line)) then do
                                                                                        newline <- pure (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) + 1) (pivotHighlight line))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    else if ((pivotHighlight line) < (rightBrackPos line)) then do
                                                                                        newline <- pure (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) + 1) (pivotHighlight line))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If highlight is getting smaller ([|....] to [|])
                                                                                    else do
                                                                                        newline <- pure (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line + 1)) (rightBrackPos line) (pivotHighlight line))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                            else if (shiftarrow == 'D') then do
                                                                                -- If there is nothing on the left, do nothing
                                                                                if (cursorPos line == 0) then
                                                                                    do
                                                                                    newline <- pure line
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                                -- If highlight is getting bigger ([|] to [|.....])
                                                                                else if ((pivotHighlight line) == (leftBrackPos line) && (pivotHighlight line) == (rightBrackPos line)) then do
                                                                                    newline <- pure (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line) - 1) (rightBrackPos line) (pivotHighlight line))
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                                else if ((pivotHighlight line) > (leftBrackPos line)) then do
                                                                                    newline <- pure (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line) - 1) (rightBrackPos line) (pivotHighlight line))
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                                -- If highlight is getting smaller ([....]| to []|)
                                                                                else do
                                                                                    newline <- pure (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) - 1) (pivotHighlight line))
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                            else if (shiftarrow == 'A') then do
                                                                                let oldCursorPos = (cursorPos line)
                                                                                -- Get formatted string
                                                                                let oldString = textContents (upDownFormatting (TextFormat "" 0 "" 0 (contents line) 0 (cursorPos line) True (leftBrackPos line) True (rightBrackPos line) True))
                                                                                -- Turn that into list of integers, each element being the length of each string
                                                                                let listLen = map plusOne (wordLengths (lines oldString))
                                                                                -- Find the line that cursor is currently on
                                                                                let currLine = getCurrLine oldCursorPos listLen
                                                                                -- If line 0, don't do anything
                                                                                if (currLine == 0) then do
                                                                                    newline <- pure line
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                                -- Else, find the new cursor position
                                                                                else do
                                                                                    -- Find column that cursor is currently on
                                                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                                                    -- Get new cursor position
                                                                                    let newCursorPos = upCursorPos currLine currColumn listLen
                                                                                    let pivot = pivotHighlight line
                                                                                    let lb = leftBrackPos line
                                                                                    let rb = rightBrackPos line
                                                                                    -- If just starting highlight, move left bracket only with new cursor (get bigger)
                                                                                    if (pivot == lb && pivot == rb) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                        --checkingPrintTELine newline
                                                                                        --checkingPrintFormat newline
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If left bracket is already moving with cursor, then keep moving it (get bigger)
                                                                                    else if (lb < pivot) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                        --checkingPrintTELine newline
                                                                                        --checkingPrintFormat newline
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If we are reducing the highlight section (get smaller) but highlight section is not yet fully removed
                                                                                    else if (lb == pivot && newCursorPos >= pivot) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                        --checkingPrintTELine newline
                                                                                        --checkingPrintFormat newline
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- Else we are highlighting from one direction to another (the brackets have to change)
                                                                                    else do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (pivot) (pivot))
                                                                                        --checkingPrintTELine newline
                                                                                        --checkingPrintFormat newline
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                            else if (shiftarrow == 'B') then do
                                                                                let oldCursorPos = (cursorPos line)
                                                                                -- Get formatted string
                                                                                let oldString = textContents (upDownFormatting (TextFormat "" 0 "" 0 (contents line) 0 (cursorPos line) True (leftBrackPos line) True (rightBrackPos line) True))
                                                                                -- Turn that into list of integers, each element being the length of each string
                                                                                let listLine = lines oldString
                                                                                let listLen = map plusOne (wordLengths listLine)
                                                                                -- Find the line that cursor is currently on
                                                                                let currLine = getCurrLine oldCursorPos listLen
                                                                                -- If last line, don't do anything
                                                                                if (currLine == ((length listLen) - 1)) then do
                                                                                    do
                                                                                        newline <- pure line
                                                                                        -- checkingPrintTELine newline
                                                                                        -- checkingPrintFormat newline
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                -- Else move down a line
                                                                                else do
                                                                                    -- Find column that cursor is currently on
                                                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                                                    -- Get new cursor position
                                                                                    let newCursorPos = min (charTotal line) (downCursorPos currLine currColumn listLen)
                                                                                    let pivot = pivotHighlight line
                                                                                    let lb = leftBrackPos line
                                                                                    let rb = rightBrackPos line
                                                                                    -- Starting new highlight - getting larger to the right
                                                                                    if (pivot == lb && pivot == rb) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If already highlighting to right, just get bigger
                                                                                    else if (rb > pivot) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If reducing the highlighting (from left to pivot), but not yet reached pivot yet
                                                                                    else if (rb == pivot && newCursorPos <= pivot) then do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                                    -- If pass the pivot, from left, to right, getting slightly bigger on right
                                                                                    else do
                                                                                        newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (pivot) (newCursorPos) (pivot))
                                                                                        printEditorView newline
                                                                                        appendToLine (pure newline)
                                                                            else
                                                                                do
                                                                                    newline <- pure line
                                                                                    printEditorView newline
                                                                                    appendToLine (pure newline)
                                                                else
                                                                    do
                                                                        newline <- pure line
                                                                        printEditorView newline
                                                                        appendToLine (pure newline)
                                                    else
                                                        do
                                                            newline <- pure line
                                                            printEditorView newline
                                                            appendToLine (pure newline)
                                        -- IF up arrow is pressed
                                        else if (numberchar == 'A') then
                                            do
                                                let oldCursorPos = (cursorPos line)
                                                -- Get formatted string
                                                let oldString = textContents (upDownFormatting (TextFormat "" 0 "" 0 (contents line) 0 (cursorPos line) True (leftBrackPos line) True (rightBrackPos line) True))
                                                -- Turn that into list of integers, each element being the length of each string
                                                -- let listLen = map plusOne (wordLengths (lines oldString))
                                                let listLine = lines oldString
                                                let listLen = map plusOne (wordLengths listLine)
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
                                                    newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (newCursorPos) (newCursorPos))
                                                    -- checkingPrintTELine newline
                                                    -- checkingPrintFormat newline
                                                    printEditorView newline
                                                    --print (listLine)
                                                    --print (listLen)
                                                    appendToLine (pure newline)
                                        else if (numberchar == 'B') then
                                            do
                                                let oldCursorPos = (cursorPos line)
                                                -- Get formatted string
                                                let oldString = textContents (upDownFormatting (TextFormat "" 0 "" 0 (contents line) 0 (cursorPos line) True (leftBrackPos line) True (rightBrackPos line) True))
                                                -- Turn that into list of integers, each element being the length of each string
                                                let listLine = lines oldString
                                                let listLen = map plusOne (wordLengths listLine)
                                                -- Find the line that cursor is currently on
                                                let currLine = getCurrLine oldCursorPos listLen
                                                -- If last line, don't do anything
                                                if (currLine == ((length listLen) - 1)) then do
                                                    newline <- pure line
                                                    -- checkingPrintTELine newline
                                                    -- checkingPrintFormat newline
                                                    printEditorView newline
                                                    --print (listLine)
                                                    --print (listLen)
                                                    appendToLine (pure newline)
                                                -- Else move down a line
                                                else do
                                                    -- Find column that cursor is currently on
                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                    -- Get new cursor position
                                                    let newCursorPos = min (charTotal line) (downCursorPos currLine currColumn listLen)
                                                    -- Find number of characters to move to stringAfterCursor
                                                    newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (newCursorPos) (newCursorPos))
                                                    --checkingPrintTELine newline
                                                    --checkingPrintFormat newline
                                                    printEditorView newline
                                                    --print (listLine)
                                                    --print (listLen)
                                                    appendToLine (pure newline)
                                        else if (numberchar == 'D') then
                                            if (cursorPos line == 0) then do
                                                newline <- pure line
                                                printEditorView newline
                                                appendToLine (pure newline)
                                            else do
                                                newline <- pure (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((cursorPos line) - 1) ((cursorPos line) - 1) ((cursorPos line) - 1))
                                                printEditorView newline
                                                appendToLine (pure newline)
                                        else if (numberchar == 'C') then do
                                            if ((cursorPos line) == (charTotal line)) then do
                                                newline <- pure line
                                                printEditorView newline
                                                appendToLine (pure newline)
                                            else do
                                                newline <- pure (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))
                                                printEditorView newline
                                                appendToLine (pure newline)
                                        else
                                            do
                                                newline <- pure line
                                                printEditorView newline
                                                appendToLine (pure newline)
                            else
                                do
                                    newline <- pure line
                                    printEditorView newline
                                    appendToLine (pure newline)
                else
                    -- If text is highlighted, replace it with this character
                    if (leftBrackPos line /= rightBrackPos line) then do
                        newline <- pure (TELine (replaceHighlight nextchar (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) ((leftBrackPos line)+1) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line)) + 1) ((leftBrackPos line)+1) ((leftBrackPos line)+1) ((leftBrackPos line)+1))
                        printEditorView newline
                        appendToLine (pure newline)
                    else do
                        newline <- pure (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))
                        printEditorView newline
                        appendToLine (pure newline)

checkCursorDelete :: Int -> Int
checkCursorDelete cursor =
    if (cursor == 0) then
        cursor
    else
        cursor - 1

-- Don't need now
takeOffN :: Integer -> String -> String
takeOffN toTakeOff afterCursor =
    if (toTakeOff == 0) then
        afterCursor
    else
        takeOffN (toTakeOff - 1) (tail afterCursor)

save :: String -> IO ()
save line = do
    saveHandle <- getAndOpenFile "Save to: " WriteMode
    let formattedText = line
        in
            hPutStr saveHandle formattedText
    hClose saveHandle
    putStrLn ("saved")

-- Don't need now
totalTELineChars :: Integer -> String -> Integer
totalTELineChars total str =
    if (str == "") then
        total
    else
        totalTELineChars (total + 1) (tail str)

load :: IO TELine
load = do
    loadHandle <- getAndOpenFile "Enter file name: " ReadMode
    contents <- hGetContents loadHandle
    -- hClose loadHandle
    return (TELine contents 0 (length contents) 0 0 0)

-- "adapted" from a Haskell tutorial
getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
    do
        putStr prompt
        name <- getLine
        catch
            (openFile name mode)
            (\(_ :: IOError) ->
                do
                    putStrLn ("Cannot open " ++ name ++ ", creating new file\n")
                    openFile name WriteMode)

getnewInput :: IO Char
getnewInput = do
    getChar

-- Don't need now
appendNewLine :: String -> String -> String
appendNewLine newlinestr line = line ++ newlinestr

-- Don't need now
appendChar :: Char -> String -> String
appendChar nextchar line = line ++ charToString nextchar

charToString :: Char -> String
charToString = (:[])

-- Don't need now
-- trying to make backspace work
shave :: [a] -> [a]
shave [] = []
shave [h]    = []
shave (h:t)  =[h]++shave t