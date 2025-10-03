{
  description = "NixOS from Scratch";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }: {

    nixosConfigurations.xsoder = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.xsoder = import ./home.nix;
            backupFileExtension = "backup";
          };
        }
      ];
    };

    homeConfigurations.xsoder = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./home.nix
        {
          home = {
            username = "xsoder";
            homeDirectory = "/home/xsoder";
            stateVersion = "25.05";
          };
        }
      ];
    };
  };
}
