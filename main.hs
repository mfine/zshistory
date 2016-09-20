{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude                     hiding (lines, mapMaybe)
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString       hiding (string)
import           Data.Attoparsec.ByteString.Char8 hiding (string)
import           Data.Conduit
import           Data.Conduit.Binary              hiding (mapM_)
import           Data.Conduit.List                hiding (foldM, map, mapM_)
import qualified Data.HashMap.Strict              as Map
import           Formatting                       hiding (char)

data Item = Item
  { timestamp :: Int
  , duration  :: Int
  , command   :: String
  } deriving Eq

instance Ord Item where
  compare a b = compare (timestamp a) (timestamp b)

fromItem :: Item -> Text
fromItem (Item t d c) =
  sformat (": " % int % ":" % int % ";" % string) t d c

toItem :: Parser Item
toItem = do
  t <- char ':' *> many1 space *> decimal
  d <- char ':' *> decimal
  c <- char ';' *> many1 anyChar
  return $ Item t d c

foldFile :: HashMap String Item -> FilePath -> IO (HashMap String Item)
foldFile itemMap file = do
  runResourceT $
    sourceFile file  =$=
      lines          =$=
      mapMaybe e     $$
      fold f itemMap
    where
      e = either (const Nothing) Just . parseOnly (toItem <* endOfInput)
      f b a = Map.insertWith g (command a) a b
        where
          g old new = if compare old new == LT then new else old

main :: IO ()
main = do
  args    <- getArgs
  itemMap <- foldM foldFile mempty $ map textToString args
  mapM_ (putStrLn . fromItem) $ sort $ Map.elems itemMap
