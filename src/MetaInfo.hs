{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module MetaInfo
  (URL
  ,getUrl
  ,FileInfo
  ,getFilePath
  ,getFileLength
  ,Info
  ,infoName
  ,infoPieceLength
  ,infoPieces
  ,infoLength
  ,infoFiles
  ,MetaInfo
  ,announce
  ,info
  ,parseMetaInfo
  ) where

import Control.Lens (Traversal', (^?))
import Control.Lens.TH (makeLenses)
import Data.BEncode (BEncode(..), bRead)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Lazy as M

--------------------------------------------------------------------------------
-- | Types
--------------------------------------------------------------------------------

newtype URL = URL { _getUrl :: ByteString} deriving (Eq, Show)
makeLenses ''URL

data FileInfo =
  FileInfo { _getFilePath :: [ByteString]
           , _getFileLength :: Integer
           } deriving (Eq, Show)
makeLenses ''FileInfo

data Info =
  Info { _infoName :: Maybe ByteString
       , _infoPieceLength :: Integer
       , _infoPieces :: ByteString
       , _infoLength :: Maybe ByteString
       , _infoFiles :: Maybe [FileInfo]
       } deriving (Eq, Show)
makeLenses ''Info

data MetaInfo =
  MetaInfo { _announce :: URL
           , _info :: Info
           } deriving (Eq, Show)
makeLenses ''MetaInfo

--------------------------------------------------------------------------------
-- | BEncode Lenses
--------------------------------------------------------------------------------

bstring :: Traversal' BEncode ByteString
bstring f (BString s) = BString <$> f s
bstring _ bv = pure bv

bint :: Traversal' BEncode Integer
bint f (BInt n) = BInt <$> f n
bint _ bv = pure bv

blist :: Traversal' BEncode BEncode
blist f (BList xs) = BList <$> traverse f xs
blist _ bv = pure bv

bkey :: String -> Traversal' BEncode BEncode
bkey k f bv@(BDict m) = case M.lookup k m of
                          Just v -> f v
                          Nothing -> pure bv
bkey _ _ bv = pure bv

--------------------------------------------------------------------------------
-- | Parsers
--------------------------------------------------------------------------------

mkFileInfo :: BEncode -> Maybe FileInfo
mkFileInfo m = do
  ps <- do
    p <- m ^? bkey "path" . blist
    case p of
      BList ps -> traverse (^? bstring) ps
      _ -> Nothing
  len <- m ^? bkey "length" . bint
  return $ FileInfo ps len

mkInfo :: BEncode -> Maybe Info
mkInfo m = do
  let name = m ^? bkey "name" . bstring
  pieces <- m ^? bkey "pieces" . bint
  n <- m ^? bkey "pieces length" . bstring
  let len = m ^? bkey "length" . bstring
  let files = do
        bl <- m ^? bkey "files"
        case bl of
          BList fs -> traverse mkFileInfo fs
          _ -> Nothing
  return $ Info name pieces n len files

mkMetaInfo :: BEncode -> Maybe MetaInfo
mkMetaInfo m = do
  announce <- URL <$> m ^? bkey "announce" . bstring
  info <- mkInfo m
  return $ MetaInfo announce info

parseMetaInfo :: ByteString -> Maybe MetaInfo
parseMetaInfo bs = bRead bs >>= mkMetaInfo
