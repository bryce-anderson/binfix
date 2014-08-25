import Data.List (isSuffixOf, sort)
import Data.Functor
import System.IO
import System.Directory
import System.Environment (getArgs, getProgName)

import qualified System.FilePath as FP


corrPos :: Integer
corrPos = 0x61c -- known offset from binary files

fieldWidth :: Integer
fieldWidth = 0x63f - corrPos -- also a known offset

-- read the current correction value for the file
readField :: Handle -> IO String
readField h = go fieldWidth where
   go :: Integer -> IO String
   go 0 = return []
   go i = do 
     c <- hGetChar h
     if c /= ' '
       then (c:) <$> go (i-1)
       else return []

-- get the correction for the specified file
getCorr :: FilePath -> IO Integer
getCorr fname = withBinaryFile fname ReadMode collect where
  collect h = do
    hSeek h AbsoluteSeek corrPos
    s <- readField h
    return $ read s 

avgCorr :: [(FilePath, Integer)] -> Integer
avgCorr xs = go xs 0 0 where
  go [] 0   _         = 0
  go [] acc c         = round $ (fromIntegral acc) / (fromIntegral c)
  go ((_,0):xs) acc c = go xs acc c -- drop zeros
  go ((_,x):xs) acc c = go xs (acc+x) (c+1)

collectCorrections :: FilePath -> IO [(FilePath, Integer)]
collectCorrections b = validFiles >>= getCorrs where
  getCorrs []     = return []
  getCorrs (x:xs) = do
    i  <- getCorr x
    is <- getCorrs xs
    return $ (x,i):is

  validFiles = do
    fs <- sort <$> getDirectoryContents b
    let fss = filter (isSuffixOf "sfrm") fs
    return $ map (FP.combine b) fss

fixCorrections :: [(FilePath, Integer)] -> IO [FilePath]
fixCorrections all = go all 0 where
  go :: [(FilePath, Integer)] -> Int -> IO [FilePath]
  go ((f,0):xs) p = do
   copyFile f (f ++ ".bak") -- make a backup
   let neighbors =  filter pred $ take (3 + (min 2 p)) $ drop (p-2) all where
                    pred (_,0) = False
                    pred _     = True
       mean = avgCorr neighbors 
   putStrLn $ "Correcting file \"" ++ f ++ "\" With neighbors " ++ show neighbors
   withBinaryFile f ReadWriteMode (fix mean)
   go(xs) (p+1) >>= \fs -> return $ f:fs
  go (_:xs) p = go xs (p+1)
  go [] _ = return []
  
  fix :: Integer -> Handle -> IO ()
  fix mean h = do
    hSeek h AbsoluteSeek corrPos
    mapM_ (hPutChar h) $ show mean

runMain :: FilePath -> IO ()
runMain basePath = do
  putStrLn "Correcting files"
  fs <- collectCorrections basePath
  fixed <- fixCorrections fs
  putStrLn $ "Fixed files: " ++ show fixed

main :: IO ()
main = getArgs >>= go where
  go []  = runMain "."
  go [p] = runMain p
  go _   = do
    name <- getProgName
    putStrLn $ "Invalid command. Args must be single path or blank\n" ++
               "Usage:\n" ++ 
               "Execute " ++ name  ++ " in directory with 0 arguments\n" ++ 
               name ++ " path/to/dir # exeute with path to data files\n"

