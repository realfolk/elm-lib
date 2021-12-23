{ }:

let

systemPkgs = import <nixpkgs> {};

realfolk = import (systemPkgs.fetchFromGitHub {
  owner = "realfolk";
  repo = "nix";
  rev = "5aa6bf8a8d07cace0ba8c26ab6c9b7116f5fbe3e";
  sha256 = "0gg09bsn2d2bklgb698g8n2k9dqi1ym1jhkjhamm00ac3azdlviw";
});

pkgs = realfolk.config.pkgSet;

projectsLib = realfolk.lib.projects { inherit pkgs; };

elmLib = projectsLib.elm { inherit pkgs; };

in

{

  shell = realfolk.lib.mkShell {
    buildInputs = builtins.concatLists [
      elmLib.pkgs.all
      [ pkgs.inotifyTools ]
    ];
    shellHook =
      ''
        #required for this nix set-up to work!
        export PROJECT="$PWD"

        #source .env file if present
        test -f "$PROJECT/.env" && source .env

        #ignore files specified in .gitignore when using fzf
        #-t only searches text files and includes empty files
        export FZF_DEFAULT_COMMAND="ag -tl"
      '';
  };

}
