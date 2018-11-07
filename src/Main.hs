{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Main (main) where

import           Data.Aeson               (ToJSON, FromJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Char8    as BC8
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           Options.Applicative
import           System.Process           (readProcess)

main :: IO ()
main = fetch =<< execParser opts

fetch :: GitHubUpstream -> IO ()
fetch g@GitHubUpstream{..} = do
  let stripNewlines = Text.filter (/= '\n')
  let unpackStatus = if pack then "" else "--unpack"
  hash <- (stripNewlines . Text.pack) <$> readProcess "nix-prefetch-url" [Text.unpack (constructUrl g)] unpackStatus
  let nixpkg = NixPkg owner repo rev hash
  let pretty = BL.toStrict (encodePretty nixpkg)
  BC8.putStrLn "\n" 
  BC8.putStrLn pretty

constructUrl :: GitHubUpstream -> Text
constructUrl GitHubUpstream{..} = mconcat
  [ "https://github.com"
  , "/"
  , owner
  , "/"
  , repo
  , "/"
  , "archive"
  , "/"
  , rev
  , ".tar.gz"
  ]

opts :: ParserInfo GitHubUpstream
opts = info (parserGitHubUpstream <**> helper)
  (  fullDesc
  <> progDesc "Fetch hash information for nixpkgs"
  <> header ""
  )

parserGitHubUpstream :: Parser GitHubUpstream
parserGitHubUpstream = GitHubUpstream
  <$> strOption
      (  long "owner"
      <> metavar "Repository Owner"
      <> value "NixOS"
      )
  <*> strOption
      (  long "repo"
      <> metavar "Repository name"
      <> value "nixpkgs"
      )
  <*> strOption
      (  long "rev"
      <> metavar "SHA-1 hash of revision"
      )
  <*> switch
      ( long "pack"
      )

data GitHubUpstream = GitHubUpstream
  { owner  :: Text
  , repo   :: Text
  , rev    :: Text
  , pack :: Bool
  }

data NixPkg = NixPkg
  { owner  :: Text
  , repo   :: Text
  , rev    :: Text
  , sha256 :: Text
  }
  deriving (Generic)

instance ToJSON NixPkg
instance FromJSON NixPkg