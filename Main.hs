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
import Data.Traversable (traverse)
import Control.Monad 
import Debug.Trace
import System.FilePath ((</>),isPathSeparator, isSearchPathSeparator,splitFileName)
import Data.Monoid
import System.Process (readProcess)

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

parseConfig :: FilePath -> IO (Err Document)
parseConfig file = doesFileExist file >>= bool (return $ return (INIDoc []))
                                               (return $ parseINI file)

getMD5Command = do
  appDir <- getAppUserDataDirectory "mozilla-extract-extensions"
  configDict <- do { tree <- parseConfig (appDir </> "config"); case tree of {
      Ok x -> return x;
      Bad s -> putStrLn s >> return (INIDoc []) }}
  putStrLn "OK. 1 2 3"
  searchPaths <- fmap (wordsBy isSearchPathSeparator) $ getEnv "PATH"
  putStrLn ("searchPaths: " ++ show (take 100 searchPaths))
  md5shellCommand <- dirFind ((=="md5sum") . reverse . takeWhile (not . isPathSeparator) . reverse) searchPaths
  case headm md5shellCommand of 
     []        -> do
          hPutStrLn stderr 
                   "Warning: Could not find md5sum program in your path.  \n\
                   \         (Consequently, newer extensions will not be  \n\
                   \          updated if they change)"
          return (\_ -> return "0")
     [shellcommand] -> do
          putStrLn ( "USING md5sum program: " ++ shellcommand )
          return (\xs -> readProcess shellcommand xs "")
  where
    wordsBy f [] = []
    wordsBy f xs = let (x,y) = break f xs in x: wordsBy f (tailm y)

main :: IO ()
main = do
  putStrLn "Begin..."
  md5sum <- getMD5Command
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
  if null pf_searchResults then do
     putStrLn $ "Error:  File not found: profiles.ini (searched " ++ mozillaD ++ ")"
     exitFailure
  else 
     putStrLn $ "Mozilla profiles.ini file: " ++ profilesini
  paths <- fmap (concatMap (prependPathifRelative profilesini) . getPaths) (liftM parseINI (readFile profilesini))
  print paths
  xpis <- dirFind (".xpi" `isSuffixOf`) paths
  mapM_ print xpis
  where
    echo = putStrLn
    prependPathifRelative file0 (x, fs) | x>0 = let (d,_) = splitFileName file0 in map (d </>) fs
    prependPathifRelative _ (_,fs)  = fs

    
