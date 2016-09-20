{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude                     hiding (lines, mapMaybe)
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString       hiding (string)
import           Data.Attoparsec.ByteString.Char8 hiding (string)
import           Data.Conduit
import           Data.Conduit.Binary              hiding (mapM_)
import           Data.Conduit.List                hiding (filter, foldM, map, mapM_)
import qualified Data.HashMap.Strict              as Map
import           Formatting                       hiding (char)
import           System.Directory

data Item = Item
  { timestamp :: Int
  , duration  :: Int
  , command   :: String
  } deriving Eq

instance Ord Item where
  compare a b = compare (timestamp a) (timestamp b)

toItem :: Parser Item
toItem = do
  t <- char ':' *> many1 space *> decimal
  d <- char ':' *> decimal
  c <- char ';' *> many1 anyChar
  return $ Item t d c

fromItem :: Item -> Text
fromItem (Item t d c) =
  sformat (": " % int % ":" % int % ";" % string) t d c

foldFile :: HashMap String Item -> FilePath -> IO (HashMap String Item)
foldFile itemMap file =
  runResourceT $
    sourceFile file  =$=
      lines          =$=
      mapMaybe e     $$
      fold f itemMap
    where
      e = either (const Nothing) Just . parseOnly (toItem <* endOfInput)
      f b a = Map.insertWith g (command a) a b
        where
          g old new = if old < new then new else old

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (map (path </>) . filter f) <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

getFiles :: [FilePath] -> IO [FilePath]
getFiles =
  foldM f mempty
  where
    f b a = do
      fe <- doesFileExist a
      if fe then return $ a : b else do
        de <- doesDirectoryExist a
        if not de then return b else do
          c <- listDirectory a
          d <- getFiles c
          return $ d <> b

main :: IO ()
main = do
  args    <- getArgs
  files   <- getFiles $ map textToString args
  itemMap <- foldM foldFile mempty files
  mapM_ (putStrLn . fromItem) $ sort $ Map.elems itemMap
