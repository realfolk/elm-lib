{
  description = "Packages and shells for developing Real Folk's Elm helper library.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    neovim.url = "github:realfolk/nix?dir=lib/packages/neovim";
    ranger.url = "github:realfolk/nix?dir=lib/packages/ranger";
    rnixLsp.url = "github:nix-community/rnix-lsp";
    elmPackages.url = "github:realfolk/nix?dir=lib/projects/elm/packages/elm-0.19";
    nodeInterpreter.url = "github:realfolk/nix?dir=lib/projects/node/interpreter/node-17";
  };

  outputs = {
    self,
    nixpkgs,
    flakeUtils,
    neovim,
    ranger,
    rnixLsp,
    elmPackages,
    nodeInterpreter,
    ...
  }:
    flakeUtils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = {
          neovim = neovim.packages.${system}.default;
          ranger = ranger.packages.${system}.default;
          node = nodeInterpreter.packages.${system}.default;
          rnixLsp = rnixLsp.defaultPackage.${system};
          elm = elmPackages.packages.${system}.elm;
          elmTest = elmPackages.packages.${system}.elm-test;
          elmFormat = elmPackages.packages.${system}.elm-format;
          elmLanguageServer = elmPackages.packages.${system}.elm-language-server;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = builtins.concatLists [
            (builtins.attrValues self.packages.${system})
            [
              pkgs.silver-searcher # ag
              pkgs.fzf
              pkgs.openssl
              pkgs.inotifyTools
            ]
          ];
          shellHook = ''
            # Load ~/.bashrc if it exists
            test -f ~/.bashrc && source ~/.bashrc

            # Source .env file if present
            test -f "$PROJECT/.env" && source .env

            # Ignore files specified in .gitignore when using fzf
            # -t only searches text files and includes empty files
            export FZF_DEFAULT_COMMAND="ag -tl"

            # Initialize $PROJECT environment variable
            export PROJECT="$PWD"
          '';
        };
      });
}
