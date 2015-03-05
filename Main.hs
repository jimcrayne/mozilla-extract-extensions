{-# LANGUAGE ViewPatterns #-}

import Data.List
import System.Environment
import System.Exit
import System.IO
import System.Directory hiding (getDirectoryContents)
import qualified System.Directory (getDirectoryContents)
import Text.LexMozillaINI
import Text.SkelMozillaINI
import Text.ParMozillaINI
import Text.AbsMozillaINI
import Text.ErrM
import Text.PrintMozillaINI
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Control.Monad 
import qualified System.Info (os)
import Debug.Trace
import System.FilePath ((</>),isPathSeparator, isSearchPathSeparator,splitFileName,dropFileName)
import qualified Filesystem.Path as F (basename)
import qualified Filesystem.Path.CurrentOS as OS (decodeString,encodeString)
import Data.Monoid
import System.Process (readProcess,callProcess)
import Text.Printf (printf)
import Control.Monad.Loops (iterateWhile)
import Debug.Trace
import Data.IORef
import qualified Data.Map.Strict as M

import Text.ParserCombinators.ReadP as P
import Text.Read

-- Note: this is already defined in base 4.6.0.0
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

dirFind :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
dirFind f (x:xs) = do 
  a <- dirFind1 f x 
  b <- dirFind f xs
  return $ a ++ b 

dirFind f [] = return []

dirFind1 :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
dirFind1 match path = do
   matches0 <- fmap (map (path </>))$ getDirCts path
   let mat = filter match matches0
   matches1 <-  dirFind match matches0
   return $ filter match (map (path </>) $ matches0 ++ matches1)


getDirCts path = doesDirectoryExist path >>= bool (return []) 
              ( fmap (filter (`notElem` [".","..","lost+found"])) 
              $ System.Directory.getDirectoryContents path
              )

-- Same as Data.Bool
bool no yes b = if b then yes else no

parseINI :: String -> Err Document
parseINI = pDocument . myLexer

getPaths :: Err Document -> [(Integer,[FilePath])]
getPaths (Ok (INIDoc grps)) = concatMap getPathGrp grps
    where
        getPathGrp (INIGrp _ kvs) = filter (not . null . snd) $
                                    map ((,) r . getPathKV) kvs
            where 
               r = case headm $ dropWhile notIsRelative kvs of
                     [INIKV (MozText "IsRelative") (INIVInteger r0)] -> r0
                     _ -> -1
               notIsRelative (INIKV (MozText "IsRelative") _ ) = False
               notIsRelative _ = True
        getPathKV (INIKV (MozText "Path") (INIVText (MozText path))) = [path]
        getPathKV _ = []
getPaths _ = []

headm (x:xs) = [x]
headm _ = []
tailm (x:xs) = xs
tailm _ = []


--insertKV :: GroupName -> KeyName -> Value-> Document -> Document
insertKV :: String -> String -> String -> Document -> Document
insertKV grp key val (INIDoc groups) = let (beforeg,atg) = break (\(INIGrp (MozText g) _) -> trace ("g=" ++ show g ++ "\n" ++ "grp=" ++ show grp) g == grp) groups in
  case trace ("beforeg=" ++ show beforeg ++  "\natg= " ++ show atg) atg of
    [] -> INIDoc (groups ++ [INIGrp (MozText grp) [INIKV (MozText key) (INIVText (MozText val))]])
    (INIGrp _ kvs :xs) -> INIDoc $ beforeg ++ (INIGrp (MozText grp) (kvs ++ [INIKV (MozText key) (INIVText (MozText val))])):xs

getKeyValue :: String -> String -> Document -> Maybe String
getKeyValue grp key (INIDoc (dropWhile (\(INIGrp (MozText g) _) -> g /= grp) -> dict)) = case dict of
  [] -> Nothing
  (INIGrp _ (dropWhile (\(INIKV (MozText k) _) -> k /= key) -> kvs):_) -> case kvs of
     (INIKV _ (INIVText (MozText value)):_) -> return value
     --(INIKV _ (INIVInteger value):_) -> return value
     _ -> Nothing
     

defaultCheckSumExeName :: String
defaultCheckSumExeName = if "mingw" `isPrefixOf` System.Info.os then "fciv" else "md5sum"

defaultUnzipName :: String
defaultUnzipName = if "mingw" `isPrefixOf` System.Info.os then "unzip.exe" else "unzip"

getDefaultConfigDocument :: IO Document
getDefaultConfigDocument = do
  checksumExePath <- liftM (fromMaybe "") (findExecutable defaultCheckSumExeName)
  unzipExePath <- liftM (fromMaybe "") (findExecutable defaultUnzipName)
  let defaultCheckSumOptions = if "mingw" `isPrefixOf` System.Info.os then "/md5" else "--"
      defaultConfigDoc = INIDoc [INIGrp (MozText "Checksum") 
                                        [ INIKV (MozText "executable") (INIVText (MozText checksumExePath))
                                        , INIKV (MozText "options") (INIVText (MozText defaultCheckSumOptions)) ]
                                ,INIGrp (MozText "Checksum") 
                                        [ INIKV (MozText "executable") (INIVText (MozText unzipExePath))
                                        , INIKV (MozText "options") (INIVText (MozText "-d")) ]
                                ]

  return defaultConfigDoc


promptSelect instruction prompt options = promptSelectLL instruction prompt options False
promptSelectLL instruction prompt options lastMeansCustom = do
  putStrLn instruction
  selection <- iterateWhile (\x -> x <=0 || x > length options) $ do
      mapM_ putStrLn (zipWith (\x y -> show x ++ ") " ++ y) [1..] options)
      putStr prompt
      hFlush stdout
      line <- getLine 
      readAnswer line
  if lastMeansCustom && selection == length options then do
    putStrLn (last options ++ ": ")
    line <- getLine
    return line
  else 
    return (options !! (selection - 1))
  where 
    readAnswer :: String -> IO Int
    readAnswer s = 
      case readEither s :: Either String Int of
        Left _ -> return (-1)
        Right x -> return x
   
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p t e = do b <- p
               if b then t else e
  
getConfigDict :: IO Document
getConfigDict = do
  defaultConfigDoc <- getDefaultConfigDocument

  appDir <- getAppUserDataDirectory "mozilla-extract-extensions"
  appconfigDir <- fmap (</> "mozilla-extract-extensions") $ getAppUserDataDirectory "config"
  appcacheDir <- fmap (</> "mozilla-extract-extensions") $ getAppUserDataDirectory "cache"
  appshareDir <- fmap ((</> "mozilla-extract-extensions"). (</> "share")) $ getAppUserDataDirectory "local"

  -- Is this the first run? - May have to create config file
  possiblyExistingConfigs <- sequence [doesFileExist (appDir </> "config"), doesFileExist (appconfigDir </> "config")]
  let existsConfig = or possiblyExistingConfigs
      configPath = case possiblyExistingConfigs of
                    [False,True] -> appconfigDir </> "config"
                    _ -> appDir </> "config"

  createdConfig <- newIORef False
  if not existsConfig then do
    putStrLn ("Could not find file: " ++ (appDir </> "config") ++ " nor file: " ++ (appconfigDir </> "config"))
    putStrLn "It will speed up future runs, if we create this file now."
    putStr "Would you like me to create it now? [Y/n] "
    hFlush stdout
    answer <- getLine
    let yesCreateIt = do { path <- promptSelect "\nThe following locations for the config file are supported, please choose one:\n" 
                                                "Enter selection [1/2] ? " 
                                                [appDir </> "config", appconfigDir </> "config"]  ;
                           checksumCachePath <- promptSelect "\nPlese select location for checksum cache file:\n"
                                                "Enter selection [1/2] ? " 
                                                [appDir </> "checksum.cache", appcacheDir </> "checksum.cache"]  ;
                           outputFolder <- promptSelectLL "\nWhere to expand sources to?\n"
                                                "Enter selction [1/2/3/4] ? "
                                                [ appDir </> "src"
                                                , appcacheDir </> "src"
                                                , appshareDir </> "src"
                                                , "Specify full path for custom location for sources"]
                                                True ;
                           createConfigFile path (insertKV "Output" "folder" outputFolder (insertKV "Checksum" "cache" checksumCachePath defaultConfigDoc));
                           writeIORef createdConfig True }
    case answer of
      '\n':_ -> yesCreateIt
      'Y':_ -> yesCreateIt
      'y':_ -> yesCreateIt
      _ -> putStrLn "Warning: Not creating config file per user request..."
  else putStrLn $ "Config file found, reading " ++ configPath ++ " ..."
  configDict <- ifM  (readIORef createdConfig)
                     (do { tree <- liftM parseINI (readFile configPath); 
                           case tree of { Ok x -> return x; Bad s -> putStrLn s >> return defaultConfigDoc }})
                     (return defaultConfigDoc) 
  return configDict

getMD5Command configDict =
  let checksumExe= fromMaybe "" (getKeyValue "Checksum" "executable" configDict)
      checksumOpt= fromMaybe "--" (getKeyValue "Checksum" "options" configDict)
   in case  checksumExe of 
     ""        -> do
          hPutStrLn stderr 
                   (printf "Warning: Could not find %s program in your path.  \n\
                   \         (Consequently, newer extensions will not be  \n\
                   \          updated if they change)" defaultCheckSumExeName)
          return (\_ -> return "0")
     shellcommand -> do
          putStrLn ( "USING md5sum program: " ++ shellcommand )
          return (\xs -> readProcess shellcommand (checksumOpt:xs) "")

getUnzipCommand configDict =
  let unzipExe= fromMaybe defaultUnzipName (getKeyValue "Output" "executable" configDict)
      unzipOpt= fromMaybe "-d" (getKeyValue "Output" "options" configDict)
   in case  unzipExe of 
     ""        -> do
          hPutStrLn stderr 
                   (printf "Warning: Could not find %s program in your path.  \n\
                   \         (Extensions will not be extracted until it is installed)" defaultUnzipName)
          return (\_ -> return ())
     shellcommand -> do
          putStrLn ( "USING md5sum program: " ++ shellcommand )
          return (\xs -> callProcess shellcommand (unzipOpt:xs) )

createConfigFile file document = do
  putStrLn $ "Creating file: " ++ file
  createDirectoryIfMissing True {- parents -} (dropFileName file)
  writeFile file . unlines .  drop 1 . lines $ printTree document
  appendFile file "\n"


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  appDir <- getAppUserDataDirectory "mozilla-extract-extensions"
  configDict <- getConfigDict
  md5sum <- getMD5Command configDict
  unzip <- getUnzipCommand configDict
  let outFolder    = fromMaybe (appDir </> "src") (getKeyValue "Output" "folder" configDict)
      md5cacheFile = fromMaybe (appDir </> "checksum.cache") (getKeyValue "Checksum" "cache" configDict)
  md5cache <- fmap M.fromList $ ifM (doesFileExist md5cacheFile)
                  (join . fmap (readIO::String -> IO [(String,String)]) $ readFile md5cacheFile)
                  (return [])
  putStrLn "current md5sums:"
  mapM_ (putStr . fst) (M.toList md5cache)
  testsum <- md5sum ["test"]
  print testsum
  homeD <- getHomeDirectory
  mozillaD <- getAppUserDataDirectory "mozilla"  
  cacheD <- getAppUserDataDirectory "cache"
  echo ("home=" ++ homeD)
  echo ("mozilla=" ++ mozillaD)
  echo ("cacheD=" ++ cacheD)
  mozDcnts <- getDirCts mozillaD
  pf_searchResults <- dirFind1 ("profiles.ini" `isSuffixOf`) mozillaD 
  let profilesini = head pf_searchResults
  profilesIniTree <- fmap ((\x -> case x of {Bad _ -> INIDoc []; Ok y -> y}) . parseINI) (readFile profilesini)
  putStrLn ("DEBUG ini:" ++ printTree profilesIniTree)
  if null pf_searchResults then do
     putStrLn $ "Error:  File not found: profiles.ini (searched " ++ mozillaD ++ ")"
     exitFailure
  else 
     putStrLn $ "Mozilla profiles.ini file: " ++ profilesini
  paths <- fmap (concatMap (prependPathifRelative profilesini) . getPaths) (liftM parseINI (readFile profilesini))
  print paths
  xpis <-  dirFind (".xpi" `isSuffixOf`) paths
  mapM_ print xpis
  checksums <- mapM (\s -> do {sum <- md5sum [s]; return (sum, s)})  xpis
  forM checksums $ \pair@(sum,file) -> 
     case M.lookup sum md5cache of
        Just file0 | file == file0 -> ifM (doesDirectoryExist (outFolder </> (OS.encodeString (F.basename (OS.decodeString file)))))
                                          (putStrLn ("* Skipping File:" ++ file ++"\n   (Checksum matched, and folder exists so assuming already expanded)") )
                                          (expandXPIFile unzip outFolder file)
        _  -> (expandXPIFile unzip outFolder file)
  createDirectoryIfMissing True {- parents -} (dropFileName md5cacheFile)
  writeFile md5cacheFile (show checksums)
  where
    echo = putStrLn
    prependPathifRelative file0 (x, fs) | x>0 = let (d,_) = splitFileName file0 in map (d </>) fs
    prependPathifRelative _ (_,fs)  = fs
    
    expandXPIFile unzip folder file = do
      putStrLn ("* Expanding File:" ++ file ++ " ...")
      let targetDir = (folder </> (OS.encodeString (F.basename (OS.decodeString file))))
      createDirectoryIfMissing True {- parents -} targetDir
      unzip [targetDir,file]

    
