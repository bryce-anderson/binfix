import Data.List (isSuffixOf)
import Data.Functor
import System.IO
import System.Directory
import System.Environment (getArgs)

import qualified System.FilePath as FP

corrPos :: Integer
corrPos = 0x61c

fieldWidth :: Integer
fieldWidth = 0x63f - corrPos

readField :: Handle -> IO String
readField h = go fieldWidth where
   -- go :: Integer -> IO String
   go 0 = return []
   go i = do 
     c <- hGetChar h
     if c /= ' '
       then (c:) <$> go (i-1)
       else return []

getCorr :: String -> IO Integer
getCorr fname = withBinaryFile fname ReadMode collect where
  collect h = do
    hSeek h AbsoluteSeek corrPos
    s <- readField h
    return $ read s 

avgCorr :: [(String, Integer)] -> Integer
avgCorr xs = go xs 0 0 where
  go [] 0   _     = 0
  go [] acc c     = round $ (fromIntegral acc) / (fromIntegral c)
  go ((_,0):xs) acc c = go xs acc c -- drop zeros
  go ((_,x):xs) acc c = go xs (acc+x) (c+1)

collectCorrections :: FilePath -> IO [(String, Integer)]
collectCorrections b = validFiles >>= getCorrs where
  getCorrs []     = return []
  getCorrs (x:xs) = do
    i  <- getCorr x
    is <- getCorrs xs
    return $ (x,i):is

  validFiles = do
    fs <- getDirectoryContents b
    let fss = filter (isSuffixOf "sfrm") fs
    return $ map (FP.combine b) fss

fixCorrections :: [(String, Integer)] -> Integer -> IO [String]
fixCorrections xs i = go xs where
  go :: [(String, Integer)] -> IO [String]
  go ((f,0):xs) = do
   copyFile f (f ++ ".bak")
   withBinaryFile f ReadWriteMode fix
   go(xs) >>= \fs -> return $ f:fs
  go (_:xs) = go xs
  go [] = return []
  
  mean = show i  

  fix :: Handle -> IO ()
  fix h = do
    hSeek h AbsoluteSeek corrPos
    mapM_ (hPutChar h) mean

runMain :: String -> IO ()
runMain basePath= do
  putStrLn "Correcting files"
  fs <- collectCorrections basePath
  putStrLn $ "Mean value: " ++ show (avgCorr fs)
  fixed <- fixCorrections fs (avgCorr fs)
  putStrLn $ "Fixed files: " ++ show fixed

main :: IO ()
main = do
  args <- getArgs
  go args where
    go []  = runMain "."
    go [x] = runMain x
    go _   = putStrLn "Invalid command. Args must be single path or blank"

