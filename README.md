grab hash information from github, for nix

```bash
Usage: grab [--owner REPO_OWNER] [--repo REPO_NAME] --rev REVISION_SHA1 [--pack]
  Fetch hash information for nixpkgs
```

examples:
```bash
$ grab --rev d570506dbbd5e397a24ffb6d3a14da4b5431b1cf

{
    "owner": "NixOS",
    "repo": "nixpkgs",
    "sha256": "1jrnspnw82dv91hmpxcqsnj7ghibsi366mi97b6lqwiz99wnsinw",
    "rev": "d570506dbbd5e397a24ffb6d3a14da4b5431b1cf"
}

$ grab --owner layer-3-communications --rev 3fd0a7f4b19162ff5cad5bd9aa0c9b30fbcd12a6

{
    "owner": "layer-3-communications",
    "repo": "nixpkgs",
    "sha256": "1ys0waiwlfh8rpdisbi0m4spck0p1gl6hjz5fgyjdq6fg44b9hyc",
    "rev": "3fd0a7f4b19162ff5cad5bd9aa0c9b30fbcd12a6"
}
```

By default, owner is "NixOS" and repo is "nixpkgs".
The fetched tar is unpacked by default. If you don't want it to be unpacked into a NAR, pass in the `--pack` flag.