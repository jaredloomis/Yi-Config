import Yi hiding (yellow, blue, green, red, cyan, super)
import Yi.Monad
import Yi.Keymap.Vim
import Yi.UI.Pango (start)
import Control.Monad (void)
import System.Environment
import System.Directory
import Data.List
import Data.List.Split
import Yi.Utils
import Yi.Process
import Yi.MiniBuffer
import Control.Concurrent.MVar
import Yi.Mode.Haskell
import System.Process
import System.IO

import Control.Monad.Reader (ask)
import Control.Monad (when)
import Control.Applicative

main :: IO ()
main = do
    -- cd to the directory containing
    -- the file in the first arg.
    getArgs >>= cdToFile . head
    yi config

-- | Set working directory to the directory
--   containing the file.
cdToFile :: FilePath -> IO ()
cdToFile f =
    if '/' `notElem` f
        then return ()
    else setCurrentDirectory $ getFileDir f

getFileDir :: FilePath -> FilePath
getFileDir = (++"/") . intercalate "/" . init . splitOn "/"

config :: Config
config = defaultVimConfig {
    -- Use Pango Frontend.
    startFrontEnd = start,
    -- Set the keymap.
    defaultKm = myKeymap,

    configUI = (configUI defaultVimConfig) {
        configTheme = solarizedTheme,
        configFontSize = Just 11,
        configWindowFill = ' ',
        configFontName = Just "Hasklig",
                         --Just "Source Code Pro",
                         --Just "Deja Vu Sans Mono",
        configScrollWheelAmount = 3,
        configLineWrap = False
        }
    }

myKeymap :: KeymapSet
myKeymap = mkKeymap $ defKeymap `override` \super _ -> super {
    v_top_level = v_top_level super ||>
        (char ';' ?>>! ghciRun),
    v_ex_cmds =
        exCmdMin "Run" (ghc $ defaultFlags++recomp) :
        exCmdMin "Runl" (ghc $ llvmFlags++recomp) :
        exCmdMin "Runp" (ghc $ parFlags++recomp) :
        exCmdMin "Runf" (ghc minimumFlags) :
        exCmdMin "Check" (const ghcModCheck) :
        exCmdMin "Type" (const genSignatures) :
        exCmdMin "TypeFast" (const miniSignature) :
        exCmdMin "Test" (const test) :
        v_ex_cmds super
    }
    where
        ghc = const . ghcRunFlags

        minimumFlags = ["-O2", "-j4", "-Wall"]

        defaultFlags = minimumFlags ++
                        ["-flate-dmd-anal",
                        "-funfolding-use-threshold=16",
                        "-fmax-simplifier-iterations=10"]
        recomp = ["-fforce-recomp"]

        llvmFlags = defaultFlags ++ ["-fllvm", "-optlc=-O4"]
        parFlags = "-threaded" : defaultFlags

-- | Shorter version of exCmd.
exCmdMin :: String -> (String -> YiM a) -> VimExCmd
exCmdMin cmd action =
    exCmd cmd (void . action) Nothing

-- | Run a GHC subprocess with given flags.
ghcRunFlags :: [String] -> YiM BufferRef
ghcRunFlags flags = do
    f <- withBuffer (gets file)
    case f of
        Nothing -> error "yi.hs: ghcRun - No current buffer."
        Just name -> startSubprocess "ghc" (name : flags) $
                const $ withOtherWindow $
                startSubprocess "./Main" [] (const $ return ())

-- | Start GHCi with current buffer.
ghciRun :: YiM BufferRef
ghciRun = ghciLoadBuffer >> ghciGet

-- | Testing stuff. Changes hilighting of Points 0-1000.
test :: YiM ()
test = withBuffer $ addOverlayB overlay
overlay :: Overlay
overlay = mkOverlay UserLayer (mkRegion (Point 0) (Point 1000)) commentStyle


-- AUTO-GEN Signatures --


-- | Generate a type signature for word under cursor.
--   Only works for identifiers GHCi can see.
--   Inserts type signature on the next line up
--   from the cursor.
genSignatures :: YiM ()
genSignatures = do
    word <- withBuffer $ readUnitB unitWord
    sigRaw <- getSignature word
    if not $ null sigRaw
        then withBuffer $ lineUp >> insertN ('\n':sigRaw)
    else
        withMinibufferFree ("Cannot detect type of \"" ++ word ++ "\".")
                           (const $ return ())

-- | Get the type of word under cursor and
--   display on Minibuffer.
miniSignature :: YiM ()
miniSignature = do
    word <- withBuffer $ readUnitB unitWord
    sigRaw <- getSignature word
    when (not $ null sigRaw) $
        withMinibufferFree sigRaw (const $ return ())

-- | Given a String global identifier, ask
--   GHCi to tell us the type signature.
--   If GHCi doesn't know, "" is returned.
getSignature :: String -> YiM String
getSignature word = do
    (Just stdin', Just stdout', Just stderr', handle) <-
        ghciProcess >>= io . createProcess
    io $ hPutStrLn stdin' $ ":t " ++ word
    ghciOut <- io $ hGetContents stdout'
    ghciErr <- io $ hGetContents stderr'

    let ghciLines = lines ghciOut
    if "parse error on input" `isInfixOf` ghciErr || not (length ghciLines >= 2)
        then return ""
    else do
        let relevantLine = ghciLines !! (length ghciLines - 2)
            parsedLine = tail $ dropWhile (/=' ') relevantLine
        return parsedLine

ghciProcess :: YiM CreateProcess
ghciProcess = do
    f <- withBuffer (gets file)
    case f of
        Nothing -> error "yi.hs: ghciProcess"
        Just name -> return $
            CreateProcess (RawCommand "ghci" [name])
                          Nothing Nothing
                          CreatePipe CreatePipe CreatePipe
                          False False False


-- GHCMOD --


data GhcModWarning =
    GhcModWarning String (Int, Int) String

-- | Call "ghc-mod check" and direct output to
--   a new buffer.
ghcModCheck :: YiM ()
ghcModCheck = do
    f <- withBuffer (gets file)
    case f of
        Nothing ->
            error "yi.hs: ghcModCheck - No current buffer."
        Just name -> do
            (exitCode, stdout, stderr) <-
                io $ runProgCommand "ghc-mod" ["check", name]
            let message = stdout
            when (not $ null message) $
                withOtherWindow . void . withEditor $
                    newBufferE (Left "ghc-mod check") (fromString message)

-- I forgot that both buffers don't scroll together,
-- otherwise this would have been a great way to display
-- "ghc-mod check" output.
{-
parseCheck :: String -> [String]
parseCheck s =
    let parsed = parseCheckOutput s
    in foldl' addMsg [] parsed

addMsg :: [String] -> GhcModWarning -> [String]
addMsg msgLines (GhcModWarning _ (line, column) err) =
    modifyLine (++"<< ("++show line++", "++show column++")"++err) line msgLines

modifyLine :: (String -> String) -> Int -> [String] -> [String]
modifyLine = modLines 1
    where
        modLines :: Int -> (String -> String) -> Int -> [String] -> [String]
        modLines i f lineNum (x:xs)
            | i == lineNum =
                f x : xs
            | otherwise =
                modLines (i+1) f lineNum xs
        modLines i f lineNum [] =
            take (lineNum-i) (cycle [""]) ++ [f ""]

parseCheckOutput :: String -> [GhcModWarning]
parseCheckOutput s =
    if null s
        then []
    else concatMap parseLine . lines $ s
    where
    parseLine line =
        let blocks = splitOn ":" line
        in if length blocks < 4 then []
            else
                let fileStr = blocks !! 0
                    lineNum = read $ blocks !! 1
                    columnNum = read $ blocks !! 2
                    errMsg = blocks !! 3
                in [GhcModWarning fileStr (lineNum, columnNum) errMsg]
-}

-- SOLARIZED --

solarizedTheme :: Theme
solarizedTheme = defaultTheme `override` \sets _ -> sets {
    baseAttributes = emptyAttributes{
        background = solarizedBase03,
        foreground = solarizedRed
        },
    selectedStyle = withBg grey,
    variableStyle = withFg solarizedBlue,
    errorStyle = withFg solarizedOrange,
    commentStyle = withFg grey,
    keywordStyle = withFg solarizedCyan,
    numberStyle = withFg solarizedCyan,
    preprocessorStyle = withFg grey,
    stringStyle = withFg grey,
    typeStyle = withFg solarizedYellow,
    dataConstructorStyle = withFg solarizedYellow,
    importStyle = withFg solarizedCyan,
    operatorStyle = withFg solarizedCyan,
    quoteStyle = withFg solarizedOrange,
    builtinStyle = withFg solarizedRed
}

solarizedBase03 :: Color
solarizedBase03 = RGB 0 43 54

solarizedBase02 :: Color
solarizedBase02 = RGB 7 54 66

solarizedBase01 :: Color
solarizedBase01 = RGB 88 110 117

solarizedBase00 :: Color
solarizedBase00 = RGB 101 123 131

solarizedBase0 :: Color
solarizedBase0 = RGB 131 148 150

solarizedBase1 :: Color
solarizedBase1 = RGB 147 161 161

solarizedBase2 :: Color
solarizedBase2 = RGB 238 232 213

solarizedBase3 :: Color
solarizedBase3 = RGB 253 246 227

solarizedYellow :: Color
solarizedYellow = RGB 181 137 0

solarizedOrange :: Color
solarizedOrange = RGB 203 75 22

solarizedRed :: Color
solarizedRed = RGB 220 50 47

solarizedMagenta :: Color
solarizedMagenta = RGB 211 54 130

solarizedViolet :: Color
solarizedViolet = RGB 108 113 196

solarizedBlue :: Color
solarizedBlue = RGB 38 139 210

solarizedCyan :: Color
solarizedCyan = RGB 42 161 152

solarizedGreen :: Color
solarizedGreen = RGB 133 153 0
