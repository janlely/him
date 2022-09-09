module Him.Args (
  CommondLineArgs(..)
  , getFilePath
  , parseCommandLineArgs
) where

import System.Environment

newtype CommondLineArgs = CommondLineArgs
  { _filePath :: Maybe FilePath }

parseCommandLineArgs :: IO CommondLineArgs
parseCommandLineArgs = do
  args <- getArgs
  case args of
    [] -> return $ CommondLineArgs Nothing
    (path:_) -> return $ CommondLineArgs  (Just path)

getFilePath :: CommondLineArgs -> Maybe FilePath
getFilePath = _filePath