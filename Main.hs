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
import System.FilePath ((</>))
import Data.Monoid
import System.FilePath
import System.Process (readProcess)

dirFind f (x:xs) = dirFind1 f x ++ dirFind f xs
dirFind f [] = []

dirFind1 :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
dirFind1 match path = do
   matches0 <- fmap (map (path </>))$ getDirCts path
   matches1 <- mapM (dirFind match) (map (path </>) matches0)
   return $ filter match (map (path </>) $ matches0 ++ concat matches1)


getDirCts path = doesDirectoryExist path >>= bool (return []) 
              ( fmap (filter (`notElem` [".",".."])) 
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
               r = case (headm $ dropWhile notIsRelative kvs) of
                     [(INIKV (MozText "IsRelative") (INIVInteger r0))] -> r0
                     _ -> -1
               notIsRelative (INIKV (MozText "IsRelative") _ ) = False
               notIsRelative _ = True
        getPathKV (INIKV (MozText "Path") (INIVText (MozText path))) = [path]
        getPathKV _ = []
getPaths _ = []

headm (x:xs) = return x
headm _ = mempty

parseConfig file = do
   tree <- parseINI file
   todo
   where todo = todo

getMD5Command = do
  appDir <- getAppUserDataDirectory "mozilla-extract-extensions"
  configDict <- bool (appDir </> "config") (return []) (parseConfig (appDir </> "config"))

  searchPaths <- fmap (wordsBy isPathSeparator) $ getEnv "PATH"
  md5shellCommand <- dirFind (=="md5sum" . reverse . dropWhile isPathSeparator . reverse) searchPath
  case mhead md5shellCommand of 
     mempty        -> do
          hPutStrLn stderr 
                   "Warning: Could not find md5sum program in your path.  \n\
                   \         (Consequently, newer extensions will not be  \n\
                   \          updated if they change)"
          return (\_ -> return "0")
     shellcommand ->
          putStrLn ("USING md5sum program: " ++ shellcommand)
          return (\xs -> readProcess shellcommand xs "")
  where
    wordsBy f xs = let (x,y) = span (not . f) xs in x: wordsBy c y
    wordsBy f [] = []

main :: IO ()
main = do
  md5sum <- getMD5Command
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
  else do
     putStrLn $ "Mozilla profiles.ini file: " ++ profilesini
  paths <- fmap (concat . map (prependPathifRelative profilesini) . getPaths) (readFile profilesini >>= return . parseINI)
  print paths
  xpis <- dirFind (".xpi" `isSuffixOf`) paths
  mapM_ print xpis
  where
    echo = putStrLn
    prependPathifRelative file0 (x, fs) | x>0 = let (d,_) = splitFileName file0 in map (d </>) fs
    prependPathifRelative _ (_,fs)  = fs

    
