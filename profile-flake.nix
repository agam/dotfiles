{
  description = "My profile defaults";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      
      # Define your package list here
      packageList = with pkgs; [
        jdk
        clojure
        leiningen
        neovim
      ];
      
      # Function to generate both individual and "all" packages
      mkPackages = list: 
        let
          # Create an attrset of individual packages
          packages = builtins.listToAttrs (map (p: { name = p.pname or p.name; value = p; }) list);
        in
          # Merge individual packages and the "all" package into a single attrset
          packages // {
            all = pkgs.buildEnv {
              name = "all-packages";
              paths = builtins.attrValues packages;
            };
          };
    in {
      # Generate packages for the specified system
      packages.${system} = mkPackages packageList;

      # Keep my existing devShell configuration
      devShells.${system} = {
        default = pkgs.mkShell {
          buildInputs = packageList;
        };
      };
    };
}
