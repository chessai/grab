grab hash information from github, for nix

Usage: grab [--owner Repository Owner] [--repo Repository name]
            --rev SHA-1 hash of revision [--pack]

By default, owner is "NixOS" and repo is "nixpkgs".
The fetched tar is unpacked by default. To make sure it isn't packed, pass in the --pack flag.