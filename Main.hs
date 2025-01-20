module Main where
import MyRandom
import Data.Char
import Control.Monad
import Data.List
import System.Environment
import System.Exit

unreservedChars = [ 'a'..'z' ] ++ [ 'A'..'Z' ] ++ [ '0'..'9' ] ++ [ '-', '.', '_', '~' ]

reservedChars = genericDelimsChars ++ subDelimsChars
genericDelimsChars = ":/?#[]@"
subDelimsChars = "!$&'()*+,;="

schemeChars = [ 'a'..'z' ] ++ [ 'A'..'Z'] ++ [ '0'..'9' ] ++ [ '+', '-', '.' ]
schemeInitChars = [ 'a'..'z' ] ++ [ 'A'..'Z' ]

hexChars = [ '0'..'9' ] ++ [ 'a'..'f' ] ++ [ 'A'..'F' ]

dummyList :: Int -> [()]
dummyList x
    | x <= 0 = []
    | otherwise = () : dummyList (x - 1)

orElse :: Maybe a -> a -> a
orElse Nothing a = a
orElse (Just a) _ = a

genValidScheme :: Random String
genValidScheme = do
    init <- randomElem schemeInitChars
    len <- randomRange (0, 10)
    list <- sequence $ map (\_ -> randomElem schemeChars) (dummyList len)
    return $ init : list

data Host = HostIPLiteral String
          | HostIPv4 String
          | HostRegName String

instance Show Host where
    show (HostIPLiteral v) = v
    show (HostIPv4 v) = v
    show (HostRegName v) = v

showStatsHost :: Host -> String
showStatsHost (HostIPLiteral v) = v
showStatsHost (HostIPv4 v) = v
showStatsHost (HostRegName v) = normalizeComponent Lower v (unreservedChars ++ subDelimsChars)

data Hier = HierAuthority (Maybe String) Host (Maybe String) [String]
          | HierAbsolute [String]
          | HierRootless [String]
          | HierEmpty

showRootlessPath :: [String] -> String
showRootlessPath = intercalate "/"

showAbemptyPath :: [String] -> String
showAbemptyPath = join . map ('/' :)

instance Show Hier where
    show HierEmpty = ""
    show (HierRootless path) = showRootlessPath path
    show (HierAbsolute path) = showAbemptyPath path
    show (HierAuthority userinfo host port path) =
        let userinfo' = ((++ "@") <$> userinfo) `orElse` "" in
        let port' = ((':' :) <$> port) `orElse` "" in
        "//" ++ userinfo' ++ show host ++ port' ++ showAbemptyPath path


genValidPcharString :: Random String -> Random String
genValidPcharString gen = do
    this <- randomPercent 80
    if this then do
        char <- gen
        next <- genValidPcharString gen
        return $ char ++ next
    else
        pure ""

genValidUserInfo :: Random (Maybe String)
genValidUserInfo = do
    empty <- randomPercent 80
    if empty then pure Nothing
    else Just <$> genValidPcharString (genValidPcharRaw ":")

genValidPort :: Random (Maybe String)
genValidPort = do
    empty <- randomPercent 30
    if empty then pure Nothing
    else do
        digit <- randomElem [ '0'..'9' ]
        next <- genValidPort
        return $ Just $ digit : (next `orElse` "")

genValidHostRegName :: Random Host
genValidHostRegName = HostRegName <$> genValidPcharString (genValidPcharRaw "")

genValidHostIPv4 :: Random Host
genValidHostIPv4 = (HostIPv4 . intercalate ".") <$> (sequence $ map (\_ -> show <$> randomRange (0, 255)) (dummyList 4))

-- TODO: implement
genValidHostIPLiteral :: Random String
genValidHostIPLiteral = undefined

genValidHost :: Random Host
genValidHost = join $ randomElem [ {-HostIPLiteral,-} genValidHostIPv4, genValidHostRegName ]

genValidAuthority :: Random Hier
genValidAuthority = do
    userInfo <- genValidUserInfo
    host <- genValidHost
    port <- genValidPort
    path <- genValidAbemptyPath
    pure $ HierAuthority userInfo host port path

genValidPercentEncoding :: Random String
genValidPercentEncoding = do
    x <- randomElem hexChars
    y <- randomElem hexChars
    return [ '%', x, y ]

genValidPcharRaw :: String -> Random String
genValidPcharRaw extra = do
    percent <- randomPercent 30
    if percent then
        genValidPercentEncoding
    else
        (\x -> [x]) <$> randomElem (extra ++ unreservedChars ++ subDelimsChars)

genValidPchar :: Random String
genValidPchar = genValidPcharRaw "@:"

genValidPathSegment :: Int -> Random String
genValidPathSegment min = do
    len <- randomRange(min, 10)
    concat <$> (sequence $ map (\_ -> genValidPchar) (dummyList len))

genValidAbemptyPath :: Random [String]
genValidAbemptyPath = do
    empty <- randomPercent 40
    if empty then
        return []
    else do
        segment <- genValidPathSegment 0
        (segment :) <$> genValidAbemptyPath

genValidAbsolutePath :: Random [String]
genValidAbsolutePath = do
    single <- randomPercent 40
    if single then
        pure [""]
    else do
        segment <- genValidPathSegment 1
        (segment :) <$> genValidAbemptyPath

genValidRootlessPath = genValidAbsolutePath

genValidHier :: Random Hier
genValidHier = join $ randomElem [ pure HierEmpty, genValidAuthority, HierAbsolute <$> genValidAbsolutePath, HierRootless <$> genValidRootlessPath ]

genValidQuery :: Random (Maybe String)
genValidQuery = do
    empty <- randomPercent 70
    if empty then pure Nothing
    else Just <$> genValidPcharString (genValidPcharRaw "@:/?")

genValidFragment :: Random (Maybe String)
genValidFragment = do
    empty <- randomPercent 70
    if empty then pure Nothing
    else Just <$> genValidPcharString (genValidPcharRaw "@:/?")

data Uri = Uri String Hier (Maybe String) (Maybe String)

instance Show Uri where
    show (Uri scheme hier query fragment) =
        let query' = (('?' :) <$> query) `orElse` "" in
        let fragment' = (('#' :) <$> fragment) `orElse` "" in
        scheme ++ ":" ++ show hier ++ query' ++ fragment'

genValidUri :: Random Uri
genValidUri = do
    scheme <- genValidScheme
    hier <- genValidHier
    query <- genValidQuery
    fragment <- genValidFragment
    return $ Uri scheme hier query fragment

data Case = Lower | Keep

condLower :: Case -> Char -> Char
condLower Keep c = c
condLower Lower c = toLower c

getHexDigitValue :: Char -> Int
getHexDigitValue c
    | elem c [ '0'..'9' ] = ord c - ord '0'
    | elem c [ 'a'..'f' ] = ord c - ord 'a' + 10
    | elem c [ 'A'..'F' ] = ord c - ord 'A' + 10
    | otherwise = 0

normalizeComponent :: Case -> String -> String -> String
normalizeComponent _ [] _ = []
normalizeComponent c ('%':x:y:xs) allowed = let hex = 16*(getHexDigitValue x) + (getHexDigitValue y)  in
                                            let safe = any (\c -> ord c == hex) allowed in
                                            (if safe then [condLower c $ chr hex] else [ '%', toUpper x, toUpper y ]) ++ normalizeComponent c xs allowed
normalizeComponent c (x:xs) allowed = condLower c x : normalizeComponent c xs allowed

showStatsPath :: [String] -> String
showStatsPath = join . map (\path -> "Path: " ++ (normalizeComponent Keep path (unreservedChars ++ subDelimsChars ++ "@:")) ++ "\n")

showStatsHier :: Hier -> String
showStatsHier HierEmpty = ""
showStatsHier (HierAbsolute path) = showStatsPath path
showStatsHier (HierRootless path) = showStatsPath path
showStatsHier (HierAuthority userinfo host port path) =
    userinfo' ++ "Host: " ++ showStatsHost host ++ "\n" ++ port' ++ showStatsPath path
        where userinfo' = normalizeComponent Keep (((\u -> "Userinfo: " ++ u ++ "\n") <$> userinfo) `orElse` "") (unreservedChars ++ subDelimsChars ++ ":")
              port' = ((\p -> "Port: " ++ p ++ "\n") <$> port) `orElse` ""

showStatsUri :: Uri -> String
showStatsUri (Uri scheme hier query fragment) =
    "Scheme: " ++ (normalizeComponent Lower scheme schemeChars) ++ "\n" ++ showStatsHier hier ++ query' ++ fragment'
        where query' = normalizeComponent Keep (((\q -> "Query: " ++ q ++ "\n") <$> query) `orElse` "") (unreservedChars ++ subDelimsChars ++ "@:/?")
              fragment' = normalizeComponent Keep (((\f -> "Fragment: " ++ f ++ "\n") <$> fragment) `orElse` "") (unreservedChars ++ subDelimsChars ++ "@:/?")

testValid :: String -> Random String
testValid title = do
    uri <- genValidUri
    pure $ title ++ "\n" ++ show uri ++ "\nVALID\n" ++ showStatsUri uri ++ "$\n\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> mainHelpMenu
        ("help":_) -> mainHelpMenu
        ("--help":_) -> mainHelpMenu
        ("uri":"gen":rest) -> mainUriGen Nothing Nothing rest
        ("uri":"valid-tests":rest) -> mainUriValidTests Nothing Nothing rest
        _ -> mainUnknownSubcommand

mainHelpMenu :: IO ()
mainHelpMenu = do
    putStrLn "This is a utility for generating various URIs"
    putStrLn ""
    putStrLn "Here are all available subcommands:"
    putStrLn ""
    putStrLn "\thelp                                      - see this message"
    putStrLn "\turi gen [--amount n] [--seed n]           - generate URIs"
    putStrLn "\turi valid-tests [--amount n] [--seed n]   - generate valid test data"

mainUnknownSubcommand :: IO ()
mainUnknownSubcommand = do
    putStrLn "Sorry, I don't know about this subcommand :( Here's the help menu for you\n"
    mainHelpMenu

mainUriGen :: (Maybe Int) -> (Maybe Rng) -> [String] -> IO ()
mainUriGen amount' s' [] = do
    let s = s' `orElse` 0
    let amount = amount' `orElse` 1
    let uris = intercalate "\n" $ map show $ seed (sequence $ map (\_ -> genValidUri) (dummyList amount)) s
    putStrLn uris
    exitWith ExitSuccess
mainUriGen amount _ ("--seed":val:rest) = mainUriGen amount (Just $ read val) rest
mainUriGen _ seed ("--amount":val:rest) = mainUriGen (Just $ read val) seed rest

mainUriValidTests :: (Maybe Int) -> (Maybe Rng) -> [String] -> IO ()
mainUriValidTests amount' s' [] = do
    let s = s' `orElse` 0
    let amount = amount' `orElse` 1
    let tests = seed (concat <$> (sequence $ map (\n -> testValid $ "Test #" ++ show n ++ ":") [ 1..amount ])) s
    putStr tests
    exitWith ExitSuccess
mainUriValidTests amount _ ("--seed":val:rest) = mainUriValidTests amount (Just $ read val) rest
mainUriValidTests _ seed ("--amount":val:rest) = mainUriValidTests (Just $ read val) seed rest
