module Main where
import MyRandom
import Data.List

unreservedChars = [ 'a'..'z' ] ++ [ 'A'..'Z' ] ++ [ '0'..'9' ] ++ [ '-', '.', '_', '~' ]

reservedChars = genericDelimsChars ++ subDelimsChars
genericDelimsChars = ":/?#[]@"
subDelimsChars = "!$&'()*+,;="

schemeChars = [ 'a'..'z' ] ++ [ 'A'..'Z'] ++ [ '0'..'9' ] ++ [ '+', '-', '.' ]
schemeInitChars = [ 'a'..'z' ] ++ [ 'A'..'Z' ]

hexChars = [ '0'..'9' ] ++ [ 'a'..'f' ] ++ [ 'A'..'F' ]

emptyList :: Int -> [()]
emptyList x
    | x <= 0 = []
    | otherwise = () : emptyList (x - 1)

genValidScheme :: Random String
genValidScheme = do
    init <- randomElem schemeInitChars
    len <- randomRange (0, 10)
    list <- sequence $ map (\_ -> randomElem schemeChars) (emptyList len)
    return $ init : list

genValidColon :: String
genValidColon = [ ':' ]

data HierType = HierAuthority | HierAbsolute | HierRootless | HierEmpty
    deriving (Show, Eq, Bounded, Enum)

genValidPcharString :: Random String -> Random String
genValidPcharString gen = do
    this <- randomPercent 80
    if this then do
        char <- gen
        next <- genValidPcharString gen
        return $ char ++ next
    else
        pure ""


genValidUserInfo :: Random String
genValidUserInfo = genValidPcharString (genValidPcharRaw ":")

genValidPort :: Random String
genValidPort = genValidPcharString ((\x -> [x]) <$> randomElem [ '0'..'9' ])

data HostType = HostIPLiteral | HostIPv4 | HostRegName

genValidHostRegName :: Random String
genValidHostRegName = genValidPcharString (genValidPcharRaw "")

genValidHostIPv4 :: Random String
genValidHostIPv4 = do
    (intercalate ".") <$> (sequence $ map (\_ -> show <$> randomRange (0, 255)) (emptyList 4))

genValidHostIPLiteral :: Random String
genValidHostIPLiteral = undefined

genValidHost :: Random String
genValidHost = do
    opt <- randomElem [ {-HostIPLiteral,-} HostIPv4, HostRegName ]
    case opt of
        HostRegName -> genValidHostRegName
        HostIPv4 -> genValidHostIPv4
        HostIPLiteral -> genValidHostIPLiteral

genValidAuthority :: Random String
genValidAuthority = do
    useUserInfo <- randomPercent 15
    usePort <- randomPercent 30
    concat <$> (sequence [ if useUserInfo then (++ "@") <$> genValidUserInfo else pure "", genValidHost, if usePort then (":" ++) <$> genValidPort else pure "" ])

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

genValidPathSegment :: Random String
genValidPathSegment = do
    len <- randomRange(0, 10)
    concat <$> (sequence $ map (\_ -> genValidPchar) (emptyList len))

genValidAbemptyPath :: Random String
genValidAbemptyPath = do
    p <- randomPercent 40
    if p then
        pure ""
    else
        concat <$> sequence [ pure "/", genValidPathSegment, genValidAbemptyPath ]

genValidAbsolutePath :: Random String
genValidAbsolutePath = do
    rest <- randomPercent 85
    if rest then do
        init <- genValidPchar
        segment <- genValidPathSegment
        path <- genValidAbemptyPath
        return $ "/" ++ init ++ segment ++ path
    else
        return "/"

genValidRootlessPath :: Random String
genValidRootlessPath = do
    init <- genValidPchar
    segment <- genValidPathSegment
    path <- genValidAbemptyPath
    return $ init ++ segment ++ path

genValidHier :: Random String
genValidHier = do
    opt <- randomElem [ HierEmpty, HierAuthority, HierAbsolute, HierRootless ]
    case opt of
        HierEmpty -> pure ""
        HierAuthority -> concat <$> sequence [ pure "//", genValidAuthority, genValidAbemptyPath ]
        HierAbsolute -> genValidAbsolutePath
        HierRootless -> genValidRootlessPath

genValidQuery :: Random String
genValidQuery = do
    empty <- randomPercent 70
    if empty then pure ""
    else
        ('?' :) <$> genValidPcharString (genValidPcharRaw "@:/?")

genValidFragment :: Random String
genValidFragment = do
    empty <- randomPercent 70
    if empty then pure ""
    else
        ('#' :) <$> genValidPcharString (genValidPcharRaw "@:/?")

genValidUri :: Random String
genValidUri = concat <$> sequence [ genValidScheme, pure genValidColon, genValidHier, genValidQuery, genValidFragment ]

main :: IO ()
main = do
    let uris = map (\s -> seed genValidUri s) [0..20]
    print uris
