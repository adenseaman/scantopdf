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

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  -- create a temporary directory
  tempDir <- liftIO $ mkdtemp "/tmp/"
  result <- runExceptT $ do
    -- make sure arguments are the right length (output file, and input directory)
    if ((length args) /= 2)
      then throwError (("Usage: " ++ progName ++ " output.pdf inputDirectory"),ExitSuccess)
      else return ()
    let [outputFile,inputDirectory] = args
    -- make sure first argument doesn't already exist
    outputFileExists <- liftIO $ fileExist outputFile
    if outputFileExists
      then throwError (("Error: output file " ++ outputFile ++ " already exists"),ExitFailure (-1))
      else return ()
    -- make sure second argument points to directory
    inputDirectoryExists <- liftIO $ fileExist inputDirectory
    if (not inputDirectoryExists)
      then throwError (("Error: input directory " ++ inputDirectory ++ " does not exist"),ExitFailure (-1))
      else return ()
    inputDirectoryStatus <- liftIO $ getSymbolicLinkStatus inputDirectory
    if (not (isDirectory inputDirectoryStatus))
      then throwError (("Error: input directory " ++ inputDirectory ++ " is not a directory"),ExitFailure (-1))
      else return ()
    -- make sure the ImageMagick "convert" program is callable
    convertFilePath <- do
      maybeConvert <- liftIO $ findExecutable "convert"
      case maybeConvert of
        Nothing -> throwError (("Error: ImageMagic utility \"convert\" cannot be found"),ExitFailure (-1))
        Just convertPath -> return $ convertPath
    -- make sure the "pdfunite" program is callable
    pdfUniteFilePath <- do
      maybePDFUnite <- liftIO $ findExecutable "pdfunite"
      case maybePDFUnite of
        Nothing -> throwError (("Error: utility \"pdfunite\" cannot be found"),ExitFailure (-1))
        Just pdfUnitePath -> return $ pdfUnitePath
    -- get listing of directory
    inputDirListing <- liftIO $ getFullPathDirectoryContents inputDirectory
    -- make sure there are 1 or 2 items in the input directory
    if (((length inputDirListing) /= 1) && ((length inputDirListing) /= 2))
      then throwError (("Error: there must be one or two directories in the input directory"),ExitFailure (-1))
      else return ()
    -- make sure those items are themselves directories
    allDirectories <- liftIO $ allFileType isDirectory inputDirListing
    if (not allDirectories)
      then throwError ("Error: not all files in the input directory are themselves directories",ExitFailure (-1))
      else return ()
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
      if (not ((oddDirLength == evenDirLength) || (oddDirLength == (evenDirLength+1))))
        then throwError ("Error: number of files in odd directory is not the same or one more than the number of files in the even directory",ExitFailure (-1))
        else return ()
      -- interlace the odd and even files together
      return $ interlace oddDirListing evenDirListing
    -- make sure each item is a file, and not a directory
    allFiles <- liftIO $ allFileType isRegularFile filesList
    if (not allFiles)
      then throwError ("Error: not all files in the image directories are files",ExitFailure (-1))
      else return ()
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
    if ((length failedConversions) /= 0)
      then throwError ("Error in converting images: " ++ (show failedConversions),ExitFailure (-1))
      else return ()
    -- call pdfunite on the temporary directory and write final PDF file to output file
    liftIO $ putStrLn "saving PDF"
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode pdfUniteFilePath ((map (\number -> tempDir </> (show number) <.> "pdf") [1..(length filesList)]) ++ [outputFile]) ""
    liftIO $ putStrLn "done"
    if exitCode /= ExitSuccess
      then throwError $ (unlines ["Error in saving images to PDF:",stdout,stderr],ExitFailure (-1))
      else return ()
    return ()
  -- recursively delete the temporary directory
  liftIO $ removeDirectoryRecursive tempDir
  case result of
    Left (errorString, exitCode) -> do
      putStrLn errorString
      exitWith exitCode
    _ -> return ()
