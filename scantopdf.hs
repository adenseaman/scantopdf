import Control.Concurrent.ParallelIO
import System.Environment
import Control.Monad.Except
import System.Process (readCreateProcessWithExitCode, shell, readProcessWithExitCode)
import System.Exit (ExitCode (..), exitWith)
import System.Posix.Files (fileExist, getSymbolicLinkStatus, isDirectory, isRegularFile, FileStatus)
import System.Directory (getDirectoryContents, removeDirectoryRecursive)
import System.FilePath.Posix ((</>),(<.>))
import Data.List (sort)
import System.Posix.Temp

-- compile this with:
-- stack ghc -- scantopdf.hs -O2 -threaded -with-rtsopts="-N"
--
-- Here's how to use this program
-- For single-sided documents, just scan the whole document forward and run this program on the resulting image directory
-- For double-sided documents:
--   scan the whole document forward, this will get the odd pages
--   if the final page is just printed on one side, remove it, otherwise your last page in the PDF will be blank
--   scan the whole document backward, this will get the even pages in reverse
--   run the program on the resulting two image directories

-- TODO: there's some bug that if you run this on a directory like "./incoming", then it won't work.  I need to improve
-- the path handling.  I might also want to make it so you have to specify the input directories.

findExecutable :: String -> IO (Maybe FilePath)
findExecutable program = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell ("which " ++ program)) ""
  if exitCode == ExitSuccess
    then return $ Just (takeWhile (flip (/=) '\n')stdout)
    else return Nothing

allFileType :: (FileStatus -> Bool) -> [FilePath] -> IO Bool
allFileType statusOperation filePathList = and <$>
  mapM (\filePath -> do
      fileExists <- fileExist filePath
      if fileExists
        then do
          fileStatus <- getSymbolicLinkStatus filePath
          return $ statusOperation fileStatus
        else return False
    ) filePathList

interlace :: [a] -> [a] -> [a]
interlace (x:xs) (y:ys) = (x:y:interlace xs ys)
interlace [] y = y
interlace x [] = x

makeBlackWhiteLevelString :: Int -> Int -> String
makeBlackWhiteLevelString blackLevel whiteLevel = (show blackLevel) ++ "%," ++ (show whiteLevel) ++ "%"

filterOutRelativeDirectories :: [FilePath] -> [FilePath]
filterOutRelativeDirectories = filter (\name -> ((name /= ".") && (name /= "..")))

getFullPathDirectoryContents :: FilePath -> IO [FilePath]
getFullPathDirectoryContents directory = ((map ((</>) directory)) . filterOutRelativeDirectories) <$> (getDirectoryContents directory)

throwErrorOnTrue :: Bool -> String -> ExitCode -> ExceptT (String,ExitCode) IO ()
throwErrorOnTrue True errorMessage exitCode = throwError (errorMessage,exitCode)
throwErrorOnTrue False _ _ = return ()

throwErrorOnTrueIO :: IO Bool -> String -> ExitCode -> ExceptT (String,ExitCode) IO ()
throwErrorOnTrueIO action errorMessage exitCode = do
  resultBool <- liftIO action
  if resultBool
    then throwError (errorMessage,exitCode)
    else return ()

throwErrorOnNothingIO :: IO (Maybe a) -> String -> ExitCode -> ExceptT (String,ExitCode) IO a
throwErrorOnNothingIO action errorMessage exitCode = do
  result <- liftIO action
  case result of
    Just x -> return x
    Nothing -> throwError (errorMessage,exitCode)

exitSuccess = ExitSuccess
exitFailure = ExitFailure (-1)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  -- create a temporary directory
  tempDir <- liftIO $ mkdtemp "/tmp/"
  result <- runExceptT $ do
    -- make sure arguments are the right length (output file, and input directory)
    throwErrorOnTrue ((length args) /= 2) ("Usage: " ++ progName ++ " output.pdf inputDirectory") exitSuccess
    let [outputFile,inputDirectory] = args
    -- make sure first argument doesn't already exist
    throwErrorOnTrueIO (fileExist outputFile) ("Error: output file " ++ outputFile ++ " already exists") exitFailure
    -- make sure second argument points to directory
    throwErrorOnTrueIO (not <$> (fileExist inputDirectory)) ("Error: input directory " ++ inputDirectory ++ " does not exist") exitFailure
    throwErrorOnTrueIO ((not . isDirectory) <$> (getSymbolicLinkStatus inputDirectory)) ("Error: input directory " ++ inputDirectory ++ " is not a directory") exitFailure
    -- make sure the ImageMagick "convert" program is callable
    convertFilePath <- throwErrorOnNothingIO (findExecutable "convert") ("Error: ImageMagick utility \"convert\" cannot be found") exitFailure
    -- make sure the "pdfunite" program is callable
    pdfUniteFilePath <- throwErrorOnNothingIO (findExecutable "pdfunite") ("Error: utility \"pdfunite\" cannot be found") exitFailure
    -- get listing of directory
    inputDirListing <- liftIO $ getFullPathDirectoryContents inputDirectory
    -- make sure there are 1 or 2 items in the input directory
    throwErrorOnTrue (((length inputDirListing) /= 1) && ((length inputDirListing) /= 2)) ("Error: there must be one or two directories in the input directory") exitFailure
    -- make sure those items are themselves directories
    throwErrorOnTrueIO (not <$> (allFileType isDirectory inputDirListing)) ("Error: not all files in the input directory are themselves directories") exitFailure
    -- combine the odd and even files together
    filesList <- do
      let (oddDirPath:evenDirPath) = map (inputDirectory </>) (sort inputDirListing)
      oddDirListing <- liftIO $ sort <$> getFullPathDirectoryContents oddDirPath
      evenDirListing <- case evenDirPath of
        [] -> return []
        -- the even directory is reversed, so reverse the files in that directory
        [evenDirPath] -> liftIO $ (reverse . sort) <$> getFullPathDirectoryContents evenDirPath
      let oddDirLength = length oddDirListing
          evenDirLength = length evenDirListing
      -- the odd directory should contain the same number, or one more, of the files than the even directory
      throwErrorOnTrue (not ((oddDirLength == evenDirLength) || (oddDirLength == (evenDirLength+1)) || (evenDirLength == 0))) ("Error: number of files in odd directory is not the same or one more than the number of files in the even directory") exitFailure
      -- interlace the odd and even files together
      return $ interlace oddDirListing evenDirListing
    -- make sure each item is a file, and not a directory
    throwErrorOnTrueIO (not <$> (allFileType isRegularFile filesList)) ("Error: not all files in the image directories are files") exitFailure
    -- process the images in parallel, calling the ImageMagick "convert" command, and output the files to sequential numbers in the temporary directory.  Stop the parallel pool right after.
    let blackLevel = 50 :: Int
        whiteLevel = 100 :: Int
        convertList = map (\(imageName, number) ->
          readProcessWithExitCode convertFilePath
            [imageName,"-level",makeBlackWhiteLevelString blackLevel whiteLevel,"-dither","None","-colors","5",tempDir </> (show number) <.> "pdf"]
            ""
          ) (zip filesList [1..])
    liftIO $ putStrLn "processing images:"
    returnValues <- liftIO $ parallel convertList
    liftIO $ putStrLn "done"
    liftIO $ stopGlobalPool
    let failedConversions = filter (\(exitCode,_,_) -> (exitCode /= ExitSuccess)) returnValues
    throwErrorOnTrue ((length failedConversions) /= 0) ("Error in converting images: " ++ (show failedConversions)) exitFailure
    -- call pdfunite on the temporary directory and write final PDF file to output file
    liftIO $ putStrLn "saving PDF"
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode pdfUniteFilePath ((map (\number -> tempDir </> (show number) <.> "pdf") [1..(length filesList)]) ++ [outputFile]) ""
    liftIO $ putStrLn "done"
    throwErrorOnTrue (exitCode /= ExitSuccess) (unlines ["Error in saving images to PDF:",stdout,stderr]) exitFailure
    return ()
  -- recursively delete the temporary directory
  liftIO $ removeDirectoryRecursive tempDir
  case result of
    Left (errorString, exitCode) -> do
      putStrLn errorString
      exitWith exitCode
    _ -> return ()
