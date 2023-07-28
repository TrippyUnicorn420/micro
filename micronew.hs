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
    , charspaceleft :: Integer
    } deriving Show

main :: IO()
main = do
    putStrLn ("Welcome to Micro. When you wish to exit, please press Ctrl+X. To save, please press Ctrl+S.")
    -- Intital Text editor, with only cursor at the start
    let currentLine = TELine "" "|" "" 120
        in do 
            printEditorView currentLine
            editorView <- appendToLine (pure currentLine)
            printEditorView editorView
    putStrLn ("\n" ++ "Thank you for using Micro.")

-- Prints the current contents of the TELine that user should see (strings only)
printEditorView :: TELine -> IO()
printEditorView editorView = do
    -- Want to clear the terminal at this point - can we use the System.Console.ANSI? Can't seem to find any built-in ways to do this.
    -- solved using printf instead of putstrln, and changing \n to \r
    -- \r is only working for one line- when you go over one line, the first line gets repeated, so it doesn't clear the whole terminal, only a line
    printf ("\r" ++ stringBeforeCursor editorView ++ cursor editorView ++ stringAfterCursor editorView)

appendToLine :: IO TELine -> IO TELine
appendToLine line =
    line
    >>= \line ->
        getnewInput
        >>= \nextchar ->
            if (nextchar == '\^X')
                then pure line
            else 
                if ((charspaceleft line) == 0) then
                    do
                    --appendToLine (pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) ((charspaceleft line) - 1)))
                        newline <- pure (TELine (appendChar (nextchar) (appendChar ('\n') (stringBeforeCursor line))) (cursor line) (stringAfterCursor line) (119))
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\n') then
                    do
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) (120))
                        printEditorView newline
                        appendToLine (pure newline)
                else if (nextchar == '\DEL') then -- for backspace
                    do
                        newline <- pure (TELine (shave (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) (charspaceleft line + 1))
                        printEditorView newline
                        appendToLine (pure newline)
                else
                    do
                    --appendToLine (pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) ((charspaceleft line) - 1)))
                        newline <- pure (TELine (appendChar nextchar (stringBeforeCursor line)) (cursor line) (stringAfterCursor line) ((charspaceleft line) - 1))
                        printEditorView newline
                        appendToLine (pure newline)

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