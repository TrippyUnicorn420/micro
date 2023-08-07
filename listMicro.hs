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

data UDTextFormat = UDTextFormat
    { udcurrWord :: String
    , udcurrWordLength :: Int
    , udcurrLine :: String
    , udtextContents :: [String]
    , udcurrTextLength :: Int
    , udtextToFormat :: String
    , udtotalCharsParsed :: Int
    } deriving Show


main :: IO()
main = do
    putStrLn ("Welcome to Micro.\nCtrl+O: open a file\tCtrl+S: save\tCtrl+X: exit without saving")
    -- Intital Text editor, with only cursor at the start
    let currentLine = TELine "" 0 0 0 0 0
        in do
            printEditorView currentLine
            editorView <- appendToLine (pure currentLine)
            printEditorView editorView
    putStrLn ("\n" ++ "Thank you for using Micro.")

-- Prints the current contents of the TELine that user should see (strings only)
printEditorView :: TELine -> IO()
printEditorView editorView = do
    putStr("\n")
    let view = (formatTELine editorView)
    printf("\ESC[2J\ESC[H\ESC[0;0f")
    putStrLn (view)

checkingPrintTELine :: TELine -> IO()
checkingPrintTELine editorView = do
    putStr("\n")
    print editorView

checkingPrintFormat :: TELine -> IO()
checkingPrintFormat editorView = do
    putStr("\n")
    print(formatting (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))

formatTELine :: TELine -> String
formatTELine editorView = textContents (formatting (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))

formatForSave :: TELine -> String
formatForSave editorView = textContents (saveFormat (TextFormat "" 0 "" 0 (contents editorView) 0 (cursorPos editorView) True (leftBrackPos editorView) True (rightBrackPos editorView) True))

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
    let lb = (lbPos te)
    let lbNotPrinted = (lbToPrint te)
    let rb = (rbPos te)
    let rbNotPrinted = (rbToPrint te)
    if (totalParsed == lb && lb /= rb && lbNotPrinted) then
        formatting (TextFormat (word ++ "[") wLength formatted fLength toFormat totalParsed cursor isNotPrinted lb False rb rbNotPrinted)
    else if (totalParsed == rb && lb /= rb && rbNotPrinted) then
        formatting (TextFormat (word ++ "]") wLength formatted fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb False)
    -- Printing cursor
    else if (totalParsed == cursor && isNotPrinted) then
        -- If not special case with 120 character word, just add | to end of word
        if (wLength /= 120) then
            formatting (TextFormat (word ++ "|") wLength formatted fLength toFormat totalParsed cursor False lb lbNotPrinted rb rbNotPrinted)
        else
            -- If 120 character word, but nothing after it, then just finish after putting | after word (no more formatting required)
            if (toFormat == "") then
                TextFormat "" 0 (formatted ++ word ++ "|") fLength toFormat totalParsed cursor False lb lbNotPrinted rb rbNotPrinted
            -- Else if a newline comes as 121st character for word, just add cursor to word now (want it before newline)
            else if (take 1 toFormat == "\n") then
                formatting (TextFormat (word ++ "|") wLength formatted fLength toFormat totalParsed cursor False lb lbNotPrinted rb rbNotPrinted)
            -- Else add in a newline and the cursor afterwards now (word will be more than 121 characters)
            -- Have to reset word to 0, so that we don't add a second newline
            else
                formatting (TextFormat "" 0 (formatted ++ word ++ "\n" ++ "|") 0 toFormat totalParsed cursor False lb lbNotPrinted rb rbNotPrinted)
    else
        -- if there is still stuff to format
        if (toFormat /= "") then do
            -- Get next character
            let newChar = take 1 toFormat
            let newWLength = wLength + 1
            -- If a space, it means the word is finished
            if (newChar == " ") then
                if (fLength + newWLength >= 120) then
                    formatting (TextFormat "" 0 (formatted ++ word ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
                else
                    formatting (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            else if (newChar == "\n") then
                formatting (TextFormat "" 0 (formatted ++ word ++ newChar) 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            else if (newWLength == 121) then
                -- slight issue with out by one error here with up arrow, but probably can leave it
                --add the current portion of the word, and a new line character
                formatting (TextFormat newChar 1 (formatted ++ word ++ "\n") 1 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            else if (fLength + newWLength > 120) then
                formatting (TextFormat (word ++ newChar) newWLength (formatted ++ "\n") 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
            else
                -- add new character to word, keep going
                formatting (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else
            TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted

appendList :: String -> [String] -> [String]
appendList a xs = xs ++ [a]

upDownFormatting :: UDTextFormat -> UDTextFormat
upDownFormatting te = do
    let word = (udcurrWord te)
    let wLength = (udcurrWordLength te)
    let line = (udcurrLine te)
    let formatted = (udtextContents te)
    let fLength = (udcurrTextLength te)
    let toFormat = (udtextToFormat te)
    let totalParsed = (udtotalCharsParsed te)
    -- if there is still stuff to format
    if (toFormat /= "") then do
        -- Get next character
        let newChar = take 1 toFormat
        let newWLength = wLength + 1
        -- If a space, it means the word is finished
        if (newChar == " ") then
            -- If word with space is 120 or more, replace 
            if (fLength + newWLength >= 120) then
                upDownFormatting (UDTextFormat "" 0 "" (appendList (line ++ word ++ newChar) formatted) 0 (tail toFormat) (totalParsed + 1))
            else
                upDownFormatting (UDTextFormat "" 0 (line ++ word ++ newChar) formatted (fLength + newWLength) (tail toFormat) (totalParsed + 1))
        else if (newChar == "\n") then
            if (toFormat /= "\n") then
                upDownFormatting (UDTextFormat "" 0 "" (appendList (line ++ word ++ newChar) formatted) 0 (tail toFormat) (totalParsed + 1))
            -- catching special case of ending in a newline (makes the line 1 long)
            else
                upDownFormatting (UDTextFormat "" 0 "" (appendList (line ++ word ++ newChar) formatted) 0 (tail toFormat) (totalParsed + 1))
        else if (newWLength == 121) then
            --add the current portion of the word, and a new line character
            upDownFormatting (UDTextFormat newChar 1 "" (appendList word formatted) 1 (tail toFormat) (totalParsed + 1))
        else if (fLength + newWLength > 120) then
            --add newline to the printText, keep word the same
            upDownFormatting (UDTextFormat (word ++ newChar) newWLength "" (appendList line formatted) 0 (tail toFormat) (totalParsed + 1))
        else
            -- add new character to word, keep going
            upDownFormatting (UDTextFormat (word ++ newChar) newWLength line formatted fLength (tail toFormat) (totalParsed + 1))
    else
        UDTextFormat "" 0 (line ++ word) (appendList (line ++ word) formatted) fLength toFormat totalParsed
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
    -- If there is still stuff to format
    if (toFormat /= "") then do
        -- Get next character
        let newChar = take 1 toFormat
        let newWLength = wLength + 1
        -- If a space, it means the word is finished, just add it to formatted string (no need for 120 character spacing here)
        if (newChar == " ") then
            saveFormat (TextFormat "" 0 (formatted ++ word ++ newChar) (fLength + newWLength) (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else if (newChar == "\n") then
            saveFormat (TextFormat "" 0 (formatted ++ word ++ newChar) 0 (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
        else
            -- add new character to word, keep going
            saveFormat (TextFormat (word ++ newChar) newWLength formatted fLength (tail toFormat) (totalParsed + 1) cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted)
    else
        TextFormat "" 0 (formatted ++ word) fLength toFormat totalParsed cursor isNotPrinted lb lbNotPrinted rb rbNotPrinted

-- Function to find the length of each string in a list
wordLengths :: [String] -> [Int]
wordLengths = map length

-- Removes the first element from a list, if possible (otherwise just stays an empty list)
removeFirst :: [a] -> [a]
removeFirst = \myList ->
    case myList of
        [] -> []
        x:xs -> xs

-- Returns line number of cursor in current line
getCurrLine :: Int -> [String] -> [Int] -> Int
getCurrLine currentPos lineStrs lineLengths = checkLineTotals 0 currentPos 0 lineLengths lineStrs

--Returns number of characters before cursor in current line
getCurrColumn :: Int -> Int -> [Int] -> Int
getCurrColumn currentPos currLine lineLengths = currentPos - (getTotals 0 (currLine - 1) lineLengths)

-- Finds total number of characters in specified number of lines
getTotals :: Int -> Int -> [Int] -> Int
getTotals total line listLines =
    -- If line is negative (exception handling)
    if (line < 0) then
        total
    else
        -- Get total number of chars for this current line and lines before
        let newTotal = (total + (listLines!!0)) in
            -- If we got to the line we needed to get to, return this total
            if (line == 0) then
                newTotal
            -- Else reduce the lines to check by 1, and go again with new total
            else
                getTotals newTotal (line - 1) (removeFirst listLines)

-- Finds line at which cursor is found
checkLineTotals :: Int -> Int -> Int -> [Int] -> [String] -> Int
checkLineTotals total cursorPos line listLengths listStrs =
    -- If there is nothing left, then we are on the line
    if (listStrs == []) then
        line
    else do
        let newTotal = (total + (listLengths!!0))
        -- If the total number of chars is more than our cursor position, we found the line
        if (newTotal > cursorPos) then
            line
        -- If the total of chars is the same
        else if (newTotal == cursorPos) then
            -- If we have a newline after, we need to go to the start of the next line, so return line + 1
            if (last(listStrs!!0) == '\n') then
                line + 1
            -- If there is more characters after as well, then return next line, not this line
            else if (length listStrs > 1) then
                line + 1
            -- If we are at the end of everything, then return this line
            else
                line
        else
            checkLineTotals newTotal cursorPos (line + 1) (removeFirst listLengths) (removeFirst listStrs)

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
upCursorPos line column listLines = (getTotals 0 (line - 2) listLines) + (min column (listLines!!(line - 1)))

-- Returns number of characters after cursor a line down
-- E.g. downCursorPos 0 3 [6, 6, 5] returns (6 + min (50, 20)) = 110
downCursorPos :: Int -> Int -> [Int] -> Int
downCursorPos line column listLines = ((getTotals 0 line listLines) + (min column (listLines!!(line + 1))))

appendToLine :: IO TELine -> IO TELine
appendToLine line =
    line
    >>= \line ->
        getnewInput
        >>= \nextchar ->
            if (nextchar == '\^X')
                then pure line
        
            else if (nextchar == '\^W') then
                do -- nothing after save
                    save (formatForSave line)
                    printLine line
            else if (nextchar == '\^O') then
                do
                    newline <- load
                    printEditorView newline
                    appendToLine (pure newline)
            else
                if (nextchar == '\n') then
                    -- If text is highlighted, replace it with this character
                    if (leftBrackPos line /= rightBrackPos line) then do
                        printLine (TELine (replaceHighlight nextchar (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) ((leftBrackPos line)+1) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line)) + 1) ((leftBrackPos line)+1) ((leftBrackPos line)+1) ((leftBrackPos line)+1))
                    else do
                        printLine (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))
                        --checkingPrintFormat(newline)
                else if (nextchar == '\DEL') then -- for backspace
                    -- If text is highlighted, delete the highlighted section
                    if (leftBrackPos line /= rightBrackPos line) then do
                        printLine (TELine (deleteHighlight (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) (leftBrackPos line) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line))) (leftBrackPos line) (leftBrackPos line) (leftBrackPos line))
                    else if ((cursorPos line) == 0) then do
                        printLine line
                    else do
                        printLine (TELine (removeCharLeft (contents line) (cursorPos line) (charTotal line)) (checkCursorDelete (cursorPos line)) ((charTotal line) - 1) (checkCursorDelete (cursorPos line)) (checkCursorDelete (cursorPos line)) (checkCursorDelete (cursorPos line)))
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
                                                            printLine (TELine (deleteHighlight (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) (leftBrackPos line) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line))) (leftBrackPos line) (leftBrackPos line) (leftBrackPos line))
                                                        -- If nothing after cursor, don't do anything
                                                        else if ((cursorPos line) == (charTotal line)) then do
                                                            printLine line
                                                        -- If something after cursor, delete first char after the cursor
                                                        else do
                                                            printLine (TELine (removeCharRight (contents line) (cursorPos line) (charTotal line)) (cursorPos line) ((charTotal line) - 1) (cursorPos line) (cursorPos line) (cursorPos line))
                                                    -- Weird keypress - don't do anything
                                                    else
                                                        do
                                                            printLine (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) (cursorPos line) (cursorPos line) (cursorPos line))
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
                                                                                        printLine line
                                                                                    -- If highlight is getting larger ([]| to [.....]|)
                                                                                    else if ((pivotHighlight line) == (leftBrackPos line) && (pivotHighlight line) == (rightBrackPos line)) then do
                                                                                        printLine (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) + 1) (pivotHighlight line))
                                                                                    else if ((pivotHighlight line) < (rightBrackPos line)) then do
                                                                                        printLine (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) + 1) (pivotHighlight line))
                                                                                    -- If highlight is getting smaller ([|....] to [|])
                                                                                    else do
                                                                                        printLine (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((leftBrackPos line + 1)) (rightBrackPos line) (pivotHighlight line))
                                                                            else if (shiftarrow == 'D') then do
                                                                                -- If there is nothing on the left, do nothing
                                                                                if (cursorPos line == 0) then
                                                                                    printLine line
                                                                                -- If highlight is getting bigger ([|] to [|.....])
                                                                                else if ((pivotHighlight line) == (leftBrackPos line) && (pivotHighlight line) == (rightBrackPos line)) then do
                                                                                    printLine (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line) - 1) (rightBrackPos line) (pivotHighlight line))
                                                                                else if ((pivotHighlight line) > (leftBrackPos line)) then do
                                                                                    printLine (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line) - 1) (rightBrackPos line) (pivotHighlight line))
                                                                                -- If highlight is getting smaller ([....]| to []|)
                                                                                else do
                                                                                    printLine (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((leftBrackPos line)) ((rightBrackPos line) - 1) (pivotHighlight line))
                                                                            else if (shiftarrow == 'A') then do
                                                                                let oldCursorPos = (cursorPos line)
                                                                                -- Get formatted string
                                                                                let listLine = udtextContents (upDownFormatting (UDTextFormat "" 0 "" [] 0 (contents line) 0))
                                                                                let listLen = wordLengths listLine
                                                                                -- Find the line that cursor is currently on
                                                                                let currLine = getCurrLine oldCursorPos listLine listLen
                                                                                -- If line 0, don't do anything
                                                                                if (currLine == 0) then do
                                                                                    printLine line
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
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                    -- If left bracket is already moving with cursor, then keep moving it (get bigger)
                                                                                    else if (lb < pivot) then do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                    -- If we are reducing the highlight section (get smaller) but highlight section is not yet fully removed
                                                                                    else if (lb == pivot && newCursorPos >= pivot) then do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                    -- Else we are highlighting from one direction to another (the brackets have to change)
                                                                                    else do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (pivot) (pivot))
                                                                            else if (shiftarrow == 'B') then do
                                                                                let oldCursorPos = (cursorPos line)
                                                                                -- Get formatted string
                                                                                let listLine = udtextContents (upDownFormatting (UDTextFormat "" 0 "" [] 0 (contents line) 0))
                                                                                let listLen = wordLengths listLine
                                                                                -- Find the line that cursor is currently on
                                                                                let currLine = getCurrLine oldCursorPos listLine listLen
                                                                                -- If last line, don't do anything
                                                                                if (currLine == ((length listLen) - 1)) then do
                                                                                    printLine line
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
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                    -- If already highlighting to right, just get bigger
                                                                                    else if (rb > pivot) then do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (lb) (newCursorPos) (pivot))
                                                                                    -- If reducing the highlighting (from left to pivot), but not yet reached pivot yet
                                                                                    else if (rb == pivot && newCursorPos <= pivot) then do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (rb) (pivot))
                                                                                    -- If pass the pivot, from left, to right, getting slightly bigger on right
                                                                                    else do
                                                                                        printLine (TELine (contents line) (newCursorPos) (charTotal line) (pivot) (newCursorPos) (pivot))
                                                                            else
                                                                                printLine line
                                                                else
                                                                    printLine line
                                                    else
                                                        printLine line
                                        -- IF up arrow is pressed
                                        else if (numberchar == 'A') then
                                            do
                                                let oldCursorPos = (cursorPos line)
                                                -- Get formatted string
                                                let listLine = udtextContents (upDownFormatting (UDTextFormat "" 0 "" [] 0 (contents line) 0))
                                                let listLen = wordLengths listLine
                                                -- Find the line that cursor is currently on
                                                let currLine = getCurrLine oldCursorPos listLine listLen
                                                -- If line 0, don't do anything
                                                if (currLine == 0) then do
                                                    printLine line
                                                -- Else move up a line
                                                else do
                                                    -- Find column that cursor is currently on
                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                    -- Get new cursor position
                                                    let newCursorPos = upCursorPos currLine currColumn listLen
                                                    newline <- pure (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (newCursorPos) (newCursorPos))
                                                    printEditorView newline
                                                    --print (listLine)
                                                    --print (listLen)
                                                    --print (currLine)
                                                    --print (currColumn)
                                                    --print (newCursorPos)
                                                    --checkingPrintTELine newline
                                                    --checkingPrintFormat newline
                                                    appendToLine (pure newline)
                                        else if (numberchar == 'B') then
                                            do
                                                let oldCursorPos = (cursorPos line)
                                                -- Get formatted list of lines
                                                let listLine = udtextContents (upDownFormatting (UDTextFormat "" 0 "" [] 0 (contents line) 0))
                                                let listLen = wordLengths listLine
                                                -- Find the line that cursor is currently on
                                                let currLine = getCurrLine oldCursorPos listLine listLen
                                                -- If last line, don't do anything
                                                if (currLine == ((length listLen) - 1)) then do
                                                    printLine line
                                                -- Else move down a line
                                                else do
                                                    -- Find column that cursor is currently on
                                                    let currColumn = getCurrColumn oldCursorPos currLine listLen
                                                    -- Get new cursor position
                                                    let newCursorPos = min (charTotal line) (downCursorPos currLine currColumn listLen)
                                                    -- Find number of characters to move to stringAfterCursor
                                                    printLine (TELine (contents line) (newCursorPos) (charTotal line) (newCursorPos) (newCursorPos) (newCursorPos))
                                        else if (numberchar == 'D') then
                                            if (cursorPos line == 0) then do
                                                printLine line
                                            else do
                                                printLine (TELine (contents line) ((cursorPos line) - 1) (charTotal line) ((cursorPos line) - 1) ((cursorPos line) - 1) ((cursorPos line) - 1))
                                        else if (numberchar == 'C') then do
                                            if ((cursorPos line) == (charTotal line)) then do
                                                printLine line
                                            else do
                                                printLine (TELine (contents line) ((cursorPos line) + 1) (charTotal line) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))
                                        else
                                            do
                                                printLine line
                            else
                                printLine line
                else
                    -- If text is highlighted, replace it with this character
                    if (leftBrackPos line /= rightBrackPos line) then do
                        printLine (TELine (replaceHighlight nextchar (contents line) (leftBrackPos line) (rightBrackPos line) (charTotal line)) ((leftBrackPos line)+1) ((charTotal line) - ((rightBrackPos line) - (leftBrackPos line)) + 1) ((leftBrackPos line)+1) ((leftBrackPos line)+1) ((leftBrackPos line)+1))
                    else do
                        printLine (TELine (addChar nextchar (contents line) (cursorPos line) (charTotal line)) ((cursorPos line) + 1) ((charTotal line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1) ((cursorPos line) + 1))


-- if we need to do something
printLine :: TELine -> IO TELine
printLine line = do
    newline <- pure line
    printEditorView newline
    appendToLine (pure newline)

checkCursorDelete :: Int -> Int
checkCursorDelete cursor =
    if (cursor == 0) then
        cursor
    else
        cursor - 1


-- creates file handle and writes to the handle to save the file
save :: String -> IO ()
save line = do
    saveHandle <- getAndOpenFile "\nSave to: " WriteMode
    let formattedText = line
        in
            hPutStr saveHandle formattedText
    hClose saveHandle
    putStrLn ("\nsaved - press anything to continue")


-- creates file handle so that it can load from a file
-- crashes if it can't find the file
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
    nextchar <- getChar
    return nextchar

charToString :: Char -> String
charToString = (:[])
