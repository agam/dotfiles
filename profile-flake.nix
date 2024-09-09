{
  description = "Personal Nix profile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages.${system} = {
        myProfile = pkgs.buildEnv {
          name = "my-profile";
          paths = with pkgs; [
            neovim
            clojure
            leiningen
            lazygit
            jdk
            go-ethereum
            kind
            k9s
            kubectl
          ];
        };
      };
    };
}
