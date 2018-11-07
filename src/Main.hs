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
  let url = Text.unpack (constructUrl g)
  let args = if pack then [url] else ["--unpack", url]
  hash <- (stripNewlines . Text.pack) <$> readProcess "nix-prefetch-url" args []
  let nixpkg = NixPkg owner repo rev hash
  let pretty = BL.toStrict (encodePretty nixpkg)
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
      <> metavar "REPO_OWNER"
      <> value "NixOS"
      )
  <*> strOption
      (  long "repo"
      <> metavar "REPO_NAME"
      <> value "nixpkgs"
      )
  <*> strOption
      (  long "rev"
      <> metavar "REVISION_SHA1"
      )
  <*> switch
      ( long "pack"
      )

data GitHubUpstream = GitHubUpstream
  { owner  :: Text
  , repo   :: Text
  , rev    :: Text
  , pack   :: Bool
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