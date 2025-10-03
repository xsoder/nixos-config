{ config, pkgs, ... }:

let
  sucklessFlake = builtins.getFlake "github:xsoder/suckless-builds";
  
  sucklessPkgs = sucklessFlake.packages.${pkgs.system};
in
{
  home.packages = [
    sucklessPkgs.dmenu
    sucklessPkgs.st
    sucklessPkgs.dwm
  ];

  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
  ];

  fonts.fontconfig.enable = true;
}
  
