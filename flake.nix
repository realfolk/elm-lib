{
  description = "A development shell for developing Real Folk's Elm helper library.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.05";
    flakeUtils.url = "github:numtide/flake-utils";

    realfolkNix = {
      url = "github:realfolk/nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flakeUtils.follows = "flakeUtils";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flakeUtils
    , realfolkNix
    , ...
    }:
    flakeUtils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      realfolkNixPkgs = realfolkNix.packages.${system};

      neovim = realfolkNixPkgs.neovim;
      nodejs = realfolkNixPkgs.nodejs;
      ranger = realfolkNixPkgs.ranger;
      rnixLsp = realfolkNixPkgs.rnixLsp;

      realfolkNixLib = realfolkNix.lib.${system};
      elmPackages = realfolkNixLib.elmPackages;
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          # Third Party
          neovim
          nodejs
          pkgs.fzf
          pkgs.inotifyTools
          pkgs.openssl
          pkgs.silver-searcher
          ranger
          rnixLsp

          # Elm
          elmPackages.elm
          elmPackages.elm-format
          elmPackages.elm-json
          elmPackages.elm-language-server
          elmPackages.elm-test
        ];

        shellHook = ''
          # Load ~/.bashrc if it exists
          test -f ~/.bashrc && source ~/.bashrc

          # Initialize environment variables
          export PROJECT="$PWD"

          # Source .env file if present
          test -f "$PROJECT/.env" && source .env

          # Ignore files specified in .gitignore when using fzf
          # -t only searches text files and includes empty files
          export FZF_DEFAULT_COMMAND="ag -tl"
        '';
      };
    });
}
