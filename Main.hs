module Main where
import MyRandom
import Control.Monad
import Data.List

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
        userinfo' ++ show host ++ port' ++ showAbemptyPath path


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
    empty <- randomPercent 50
    if empty then pure Nothing
    else Just <$> genValidPcharString ((\x -> [x]) <$> randomElem [ '0'..'9' ])

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
        (++ [segment]) <$> genValidAbemptyPath

genValidAbsolutePath :: Random [String]
genValidAbsolutePath = do
    single <- randomPercent 40
    if single then
        pure [""]
    else do
        segment <- genValidPathSegment 1
        (++ [segment]) <$> genValidAbemptyPath

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

main :: IO ()
main = do
    let uris = map (\s -> seed genValidUri s) [0..20]
    printUris uris

printUris :: [Uri] -> IO ()
printUris [] = return ()
printUris (x:xs) = do
    putStrLn $ show x
    printUris xs
